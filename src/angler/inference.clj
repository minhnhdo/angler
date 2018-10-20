(ns angler.inference
  (:require [anglican.runtime :refer [uniform-continuous exp normal observe*
                                      sample*]]
            [angler.autodiff :refer [autodiff]]
            [angler.errors :refer [check-error checked-> query-error]]
            [angler.passes.compile.clojure-function :refer [compile-to-function]]
            [angler.passes.compile.graph :refer [compile-to-graph]]
            [angler.passes.desugar :refer [desugar]]
            [angler.passes.scope :refer [scope]]
            [angler.passes.validate :refer [validate]]
            [angler.types :refer [ancestral-ordering bind-free-variables
                                  free-vars peval sample-from-prior]])
  (:import [angler.types Graph]
           [clojure.lang IFn IPersistentList IPersistentMap IPersistentVector
            Keyword Symbol]))

(defn- accept
  [^IPersistentMap P ^Symbol x ^IPersistentVector args
   ^IPersistentVector dependents ^IFn func ^IPersistentMap new-chi
   ^IPersistentMap chi]
  (let [d (apply func (map chi args))
        new-d (apply func (map new-chi args))
        loga (- (observe* new-d (chi x)) (observe* d (new-chi x)))]
    (exp (apply +
                loga
                (map #(let [[vargs _ vfunc] (P %)]
                        (- (observe* (apply vfunc (map new-chi vargs))
                                     (new-chi %))
                           (observe* (apply vfunc (map chi vargs)) (chi %))))
                     dependents)))))

(defn- gibbs-step
  ^IPersistentMap
  [^IPersistentMap P ^IPersistentMap X ^IPersistentMap sub]
  (loop [to-do X
         chi sub]
    (if (seq to-do)
      (let [[[x [args dependents func]] & new-to-do] to-do
            dist (apply func (map chi args))
            new-chi (assoc chi x (sample* dist))
            a (accept P x args dependents func new-chi chi)
            u (sample* (uniform-continuous 0 1))]
        (recur new-to-do (if (< u a) new-chi chi)))
      chi)))

(defn- gibbs-infinite-sequence
  [^IPersistentMap P ^IPersistentMap X ^IPersistentMap chi]
  (let [new-chi (gibbs-step P X chi)]
    (cons new-chi (lazy-seq (gibbs-infinite-sequence P X new-chi)))))

(defn- gibbs
  [^Graph {:keys [P Y] :as graph} & _]
  (let [ordering (ancestral-ordering graph)
        P-func (into {}
                     (map #(let [[x e] %
                                 vars (disj (free-vars {} e) x)
                                 args (vec (filter vars ordering))]
                             [x [args
                                 (binding [*ns* (in-ns 'angler.primitives)]
                                   (eval (list 'fn args
                                               (bind-free-variables Y e))))]])
                          P))
        P-dependents (into {}
                           (map (fn [[x [a f]]]
                                  [x [a
                                      (conj (mapv first
                                                  (filter (fn [[_ [args _]]]
                                                            (some #(= x %)
                                                                  args))
                                                          P-func))
                                            x)
                                      f]])
                                P-func))
        X (apply dissoc P-dependents (keys Y))
        chi (into (sample-from-prior graph ordering) Y)]
    (gibbs-infinite-sequence P-dependents X chi)))

(defn- leapfrog
  [^IPersistentList U ^IPersistentMap chi ^IPersistentMap R t epsilon]
  (let [args (second U)
        R-half (into {} (map (fn [[x v]]
                               [x (- (R x) (* 0.5 epsilon v))])
                             (nth (autodiff U (map chi args)) 1)))]
    (loop [i 1
           new-chi chi
           new-R R-half]
      (let [new-new-chi (into {} (map (fn [[x v]]
                                        [x (+ v (* epsilon (new-R x)))])
                                      new-chi))
            d (nth (autodiff U (map new-new-chi args)) 1)]
        (if (< i t)
          (recur (+ i 1)
                 new-new-chi
                 (into {} (map (fn [[x v]] [x (- v (* epsilon (d x)))])
                               new-R)))
          [new-new-chi
           (into {} (map (fn [[x v]] [x (- v (* 0.5 epsilon (d x)))])
                         new-R))])))))

(defn- hamiltonian
  [^IPersistentVector args ^IFn U-func ^IPersistentMap chi ^IPersistentMap R
   stddev]
  (+ (* 0.5 (apply + (map (fn [[_ v]] (/ (* v v) (* stddev stddev))) R)))
     (apply U-func (map chi args))))

(defn- hmc-infinite-sequence
  [^IPersistentList [_ args :as U] ^IFn U-func ^IPersistentMap chi t epsilon
   stddev normal-dist]
  (let [R (into {} (map #(vector (nth % 0) (sample* normal-dist)) chi))
        [new-chi new-R] (leapfrog U chi R t epsilon)
        u (sample* (uniform-continuous 0 1))
        selected-chi (if (< u (exp (- (hamiltonian
                                        args U-func chi R stddev)
                                      (hamiltonian
                                        args U-func new-chi new-R stddev))))
                       new-chi
                       chi)]
    (cons selected-chi
          (lazy-seq
            (hmc-infinite-sequence
              U U-func selected-chi t epsilon stddev normal-dist)))))

(defn- fix-up-expression
  [^Symbol x expr]
  (cond
    (and (list? expr) (= 'normal (first expr)))
    (apply list 'normpdf x (map #(fix-up-expression x %) (rest expr)))

    (map? expr) (into {} (map #(fix-up-expression x %) expr))

    (seqable? expr) (apply (cond
                             (vector? expr) vec
                             (set? expr) set
                             :else list)
                           (map #(fix-up-expression x %) expr))

    :else expr))

(defn- hmc
  [^Graph {:keys [P Y] :as graph}
   & {:keys [epsilon stddev t] :or {epsilon 0.1, stddev 1, t 10}}]
  (let [ordering (ancestral-ordering graph)
        normal-dist (normal 0 stddev)
        chi (sample-from-prior graph ordering)
        U (list 'fn (vec (filter chi ordering))
                (list '-
                      0
                      (reduce-kv (fn [acc x e]
                                   (list '+ (bind-free-variables
                                              Y (fix-up-expression x e))
                                         acc))
                                 0
                                 P)))
        U-func (binding [*ns* (in-ns 'angler.primitives)] (eval U))]
    (hmc-infinite-sequence U U-func chi t epsilon stddev normal-dist)))

(def ^:private algorithms
  {:gibbs gibbs
   :hmc hmc})

(defn query
  [^Keyword algorithm ^IPersistentVector program & options]
  (let [alg (algorithms algorithm)
        output (checked->
                 program
                 check-error validate
                 check-error scope
                 check-error desugar
                 check-error compile-to-graph)
        {:keys [burn-in] :or {burn-in 10000}} options]
    (cond
      (nil? alg) (query-error "Unknown algorithm " algorithm)
      (:angler.errors/error output) output
      :else (let [[graph result] output]
              (map #(peval (bind-free-variables % result))
                   (drop burn-in (apply alg graph options)))))))

(defn interp
  [^IPersistentVector program & {:keys [burn-in] :or {burn-in 10000}}]
  (let [output (checked->
                 program
                 check-error validate
                 check-error scope
                 check-error desugar
                 check-error compile-to-function)]
    (if (:angler.errors/error output)
      output
      (drop burn-in
            (repeatedly #(let [log-weight (atom 0)]
                           [(output log-weight) (deref log-weight)]))))))

(def p1
  '[(let [mu (sample (normal 1 (sqrt 5)))
          sigma (sqrt 2)
          lik (normal mu sigma)]
      (observe lik 8)
      (observe lik 9)
      mu)])

(def p2
  '[(defn observe-data [_ data slope bias]
      (let [xn (first data)
            yn (second data)
            zn (+ (* slope xn) bias)]
        (observe (normal zn 1.0) yn)
        (rest (rest data))))

    (let [slope (sample (normal 0.0 10.0))
          bias  (sample (normal 0.0 10.0))
          data (vector 1.0 2.1 2.0 3.9 3.0 5.3
                       4.0 7.7 5.0 10.2 6.0 12.9)]
      (loop 6 data observe-data slope bias)
      (vector slope bias))])

(def p3
  '[(let [data [1.1 2.1 2.0 1.9 0.0 -0.1 -0.05]
          likes (foreach 3 []
                         (let [mu (sample (normal 0.0 10.0))
                               sigma (sample (gamma 1.0 1.0))]
                           (normal mu sigma)))
          pi (sample (dirichlet [1.0 1.0 1.0]))
          z-prior (discrete pi)
          z (foreach 7 [y data]
                     (let [z (sample z-prior)]
                       (observe (nth likes z) y)
                       z))]
      (= (first z) (second z)))])

(def p4
  '[(let [sprinkler true
          wet-grass true
          is-cloudy (sample (flip 0.5))
          is-raining (if (= is-cloudy true)
                       (sample (flip 0.8))
                       (sample (flip 0.2)))
          sprinkler-dist (if (= is-cloudy true)
                           (flip 0.1)
                           (flip 0.5))
          wet-grass-dist (if (and (= sprinkler true)
                                  (= is-raining true))
                           (flip 0.99)
                           (if (and (= sprinkler false)
                                    (= is-raining false))
                             (flip 0.0)
                             (if (or (= sprinkler true)
                                     (= is-raining true))
                               (flip 0.9))))]
      (observe sprinkler-dist sprinkler)
      (observe wet-grass-dist wet-grass)
      is-raining)])

(def p5
  '[(let [x (sample (normal 0 10))
          y (sample (normal 0 10))]
      (observe (dirac (+ x y)) 7)
      [x y])])

(def hmc-p3
  '[(let [x (sample (normal 0 10))
          y (sample (normal 0 10))]
      (observe (normal (+ x y) 1E-2) 7)
      [x y])])
