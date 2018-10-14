(ns angler.inference
  (:require [anglican.runtime :refer [uniform-continuous exp observe* sample*]]
            [angler.errors :refer [check-error checked-> query-error]]
            [angler.passes.compile :refer [compile-to-graph]]
            [angler.passes.desugar :refer [desugar]]
            [angler.passes.scope :refer [scope]]
            [angler.passes.validate :refer [validate]]
            [angler.types :refer [ancestral-ordering bind-free-variables
                                  free-vars peval sample-from-prior]])
  (:import [angler.types Graph]
           [clojure.lang IPersistentMap IPersistentVector Keyword]))

(defn- markov-blanket
  [^IPersistentMap m x]
  (conj (set (map first
                  (filter #(let [[_ e] %]
                             (contains? (free-vars {} e) x))
                          m)))
        x))

(defn- accept
  [^IPersistentMap P x e ^IPersistentMap new-chi ^IPersistentMap chi]
  (let [d (peval (bind-free-variables chi e))
        new-d (peval (bind-free-variables new-chi e))
        loga (- (observe* new-d (chi x)) (observe* d (new-chi x)))
        Vx (markov-blanket P x)]
    (exp (apply +
                loga
                (map #(let [ve (P %)]
                        (- (observe* (peval (bind-free-variables new-chi ve))
                                     (new-chi %))
                           (observe* (peval (bind-free-variables chi ve))
                                     (chi %))))
                     Vx)))))

(defn- gibbs-step
  [^IPersistentMap P ^IPersistentMap X ^IPersistentMap sub]
  (loop [to-do X
         chi sub]
    (if (seq to-do)
      (let [[[x e] & new-to-do] to-do
            dist (peval (bind-free-variables chi e))
            new-chi (assoc chi x (sample* dist))
            a (accept P x e new-chi chi)
            u (sample* (uniform-continuous 0 1))]
        (recur new-to-do (if (< u a) new-chi chi)))
      chi)))

(defn- gibbs-sampling
  ([^Graph {:keys [P Y] :as graph}]
   (let [P-dist (into {} (map #(let [[k [_ dist _]] %] [k dist]) P))
         X (apply dissoc P-dist (keys Y))
         chi (into (sample-from-prior graph) Y)]
     (lazy-seq (cons chi (gibbs-sampling P-dist X chi)))))
  ([^IPersistentMap P ^IPersistentMap X ^IPersistentMap chi]
   (let [new-chi (gibbs-step P X chi)]
     (lazy-seq (cons new-chi (gibbs-sampling P X new-chi))))))

(def ^:private algorithms
  {:gibbs gibbs-sampling})

(defn query
  [^Keyword algorithm ^IPersistentVector program]
  (let [alg (algorithms algorithm)
        output (checked->
                 program
                 check-error validate
                 check-error scope
                 check-error desugar
                 check-error compile-to-graph)]
    (cond
      (nil? alg) (query-error "Unknown algorithm " algorithm)
      (:angler.errors/error output) output
      :else (let [[^Graph {:keys [P] :as graph} result] output]
              (map
                (fn [^IPersistentMap sub]
                  {:result (bind-free-variables sub result)
                   :log-weight (apply +
                                      (map
                                        #(let [[k [_ dist _]] %]
                                           (observe*
                                             (peval
                                               (bind-free-variables sub dist))
                                             (sub k)))
                                        P))})
                (alg graph))))))

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
          z-prior (discrete pi)]
      (foreach 7 [y data]
               (let [z (sample z-prior)]
                 (observe (get likes z) y)
                 (= (first z) (second z)))))])

(def p4
  '[(let [sprinkler true
          wet-grass true
          is-cloudy (sample (flip 0.5))

          is-raining (if (= is-cloudy true )
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
