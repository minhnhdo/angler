(ns angler.inference
  (:require [anglican.runtime :refer [uniform-continuous exp normal observe*
                                      sample*]]
            [angler.autodiff :refer [autodiff]]
            [angler.errors :refer [check-error checked-> query-error]]
            [angler.passes.compile :refer [compile-to-graph]]
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
    (lazy-seq (cons new-chi (gibbs-infinite-sequence P X new-chi)))))

(defn- gibbs
  [^Graph {:keys [P Y] :as graph} & options]
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
  [ordering ^IPersistentList U ^IPersistentMap chi ^IPersistentMap R t epsilon]
  (let [R-half (into {} (map (fn [[x v]]
                               [x (- (R x) (* 0.5 epsilon v))])
                             (nth (autodiff U (map chi ordering)) 1)))]
    (loop [i t
           new-chi chi
           new-R R-half]
      (if (> 1 i)
        (let [d (nth (autodiff U (map new-chi ordering)) 1)]
          (recur (- i 1)
                 (into {} (map (fn [[x v]] [x (+ v (* epsilon (new-R x)))])
                               new-chi))
                 (into {} (map (fn [[x v]] [x (- v (* epsilon (d x)))])
                               new-R))))
        (let [d (nth (autodiff U (map new-chi ordering)) 1)]
          [(into {} (map (fn [[x v]] [x (+ v (* epsilon (new-R x)))])
                         new-chi))
           (into {} (map (fn [[x v]] [x (- v (* 0.5 epsilon (d x)))])
                         R))])))))

(defn- hamiltonian
  [^IPersistentMap X ^IPersistentMap chi ^IPersistentMap R ^double stddev]
  (apply +
         (* 0.5 (apply + (map (fn [[_ v]] (/ (* v v) stddev)) R)))
         (map (fn [[x [args _ func]]]
                (- (observe* (apply func (map chi args)) (chi x))))
              X)))

(defn- hmc-infinite-sequence
  [ordering ^IPersistentMap P ^IPersistentMap X ^IPersistentList U
   ^IPersistentMap chi t epsilon stddev normal-dist]
  (let [R (into {} (map #(vector (nth % 0) (sample* normal-dist)) X))
        [new-chi new-R] (leapfrog ordering U chi R t epsilon)
        u (sample* (uniform-continuous 0 1))
        selected-chi (if (< u (exp (- (hamiltonian X chi R stddev)
                                      (hamiltonian X new-chi new-R stddev))))
                       new-chi
                       chi)]
    (lazy-seq selected-chi
              (hmc-infinite-sequence
                ordering P X U selected-chi t epsilon stddev normal-dist))))

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
   & {:keys [epsilon t stddev] :or {epsilon 0.1, stddev 1, t 10}}]
  (let [ordering (ancestral-ordering graph)
        P-func (into {}
                     (map #(let [[x e] %
                                 vars (disj (free-vars {} e) x)
                                 args (vec (filter vars ordering))]
                             [x [args
                                 e
                                 (binding [*ns* (in-ns 'angler.primitives)]
                                   (eval (list 'fn args e)))]])
                          P))
        normal-dist (normal 0 stddev)
        X (apply dissoc P-func (keys Y))
        chi (sample-from-prior graph ordering)
        U (list 'fn (vec (filter X ordering))
                (reduce-kv (fn [acc x [_ e]]
                             (list '+ (bind-free-variables
                                        Y (fix-up-expression x e))
                                   acc))
                           0
                           X))]
    (hmc-infinite-sequence
      ordering P-func X U chi t epsilon stddev normal-dist)))

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
