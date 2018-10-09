(ns angler.autodiff
  (:require [angler.types :refer [built-ins]])
  (:import [clojure.lang IPersistentList IPersistentMap ISeq]))

(def ^:private supported-operations
  {'+ {:func (get built-ins '+)
       :deriv-fns [(fn [a b] 1) (fn [a b] 1)]}
   '- {:func (get built-ins '-)
       :deriv-fns [(fn [a b] 1) (fn [a b] -1)]}
   '* {:func (get built-ins '*)
       :deriv-fns [(fn [a b] b) (fn [a b] a)]}
   '/ {:func (get built-ins '/)
       :deriv-fns [(fn [a b] (/ 1 b)) (fn [a b] (* a (/ -1 (* b b))))]}
   'exp {:func (get built-ins 'exp)
         :deriv-fns [(fn [a] ((get built-ins 'exp) a))]}
   'log {:func (get built-ins 'log)
         :deriv-fns [(fn [a] (/ 1 a))]}
   'sqrt {:func (get built-ins 'sqrt)
          :deriv-fns [(fn [a] (/ 1 (* 2 ((get built-ins 'sqrt) a))))]}
   'relu {:func (fn [a] (max 0 a))
          :deriv-fns [(fn [a] (if (> a 0) 1 0))]}
   'sin {:func (get built-ins 'sin)
         :deriv-fns [(fn [a] ((get built-ins 'cos) a))]}
   'cos {:func (get built-ins 'cos)
         :deriv-fns [(fn [a] (- ((get built-ins 'sin) a)))]}
   'normpdf {:func (fn [x mu sigma]
                     (let [x-mu (- x mu)
                           two-variance (* 2 sigma sigma)]
                       ((get built-ins 'log)
                        (* (/ 1 ((get built-ins 'sqrt) (* Math/PI two-variance)))
                           ((get built-ins 'exp) (- (/ (* x-mu x-mu)
                                                       two-variance)))))))
             :deriv-fns [(fn [x mu sigma]
                           (/ (- mu x)
                              (* sigma sigma)))
                         (fn [x mu sigma]
                           (/ (- x mu)
                              (* sigma sigma)))
                         (fn [x mu sigma]
                           (/ (- (* (- x mu) (- x mu))
                                 (* sigma sigma))
                              (* sigma sigma sigma)))]}
   '= {:func (get built-ins '=)}
   '> {:func (get built-ins '>)}
   '< {:func (get built-ins '<)}
   '>= {:func (get built-ins '>=)}
   '<= {:func (get built-ins '<=)}
   'not {:func (get built-ins 'not)}})

(defn- autodiff-forward
  ^IPersistentMap
  [^IPersistentMap sub exp]
  ;; repr {:fn-name fn-name, :args [args...], :primal primal}
  ;;   or {:is-const? true, :primal const}
  ;;   or {:is-arg? true, :arg arg, :primal const}
  (cond
    (and (list? exp)
         (= 'if (first exp))) (let [[_ cond-exp then-exp else-exp] exp]
                                (if (:primal (autodiff-forward sub cond-exp))
                                  (autodiff-forward sub then-exp)
                                  (autodiff-forward sub else-exp)))
    (list? exp) (let [[fn-name & params] exp
                      args (map #(autodiff-forward sub %) params)
                      real-args (map :primal args)]
                  {:fn-name fn-name
                   :args args
                   :primal (apply (get-in supported-operations [fn-name :func])
                                  real-args)})
    (symbol? exp) {:is-arg? true
                   :arg exp
                   :primal (get sub exp)}
    :else {:is-const? true
           :primal exp}))

(defn- autodiff-backward
  ^IPersistentMap
  [^IPersistentMap graph deriv]
  (if (or (:is-arg? graph) (:is-const? graph))
    (assoc graph :deriv deriv)
    (let [{:keys [fn-name args]} graph
          primal-args (map :primal args)
          deriv-fns (get-in supported-operations [fn-name :deriv-fns])
          derivs (map #(* deriv (apply % primal-args)) deriv-fns)
          new-args (map #(autodiff-backward %1 %2) args derivs)]
      (assoc graph :args new-args :deriv deriv))))

(defn- collect-args
  ^IPersistentMap
  [^IPersistentMap graph]
  (cond
    (:is-arg? graph) {(:arg graph) (:deriv graph)}
    (:fn-name graph) (apply merge-with + (map collect-args (:args graph)))
    :else {}))

(defn autodiff
  [^IPersistentList f ^ISeq args]
  (let [[_ params body] f
        sub (apply hash-map (interleave params args))
        graph (autodiff-backward (autodiff-forward sub body)1)]
    [(:primal graph) (merge (into {} (map #(vector % 0) params))
                            (collect-args graph))]))

(def p1
  '(fn [x]
     (exp (sin x))))

(def p2
  '(fn [x y]
     (+ (* x x) (sin x))))

(def p3
  '(fn [x]
     (if (> x 5)
       (* x x)
       (+ x 18))))

(def p4
  '(fn [x]
     (log x)))

(def p5
  '(fn [x mu sigma]
     (+ (- 0 (/ (* (- x mu) (- x mu))
                (* 2 (* sigma sigma))))
        (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma))))))))

(def p6
  '(fn [x mu sigma]
     (normpdf x mu sigma)))

(def p7
  '(fn [x1 x2 x3]
     (+ (+ (normpdf x1 2 5)
           (if (> x2 7)
             (normpdf x2 0 1)
             (normpdf x2 10 1)))
        (normpdf x3 -4 10))))
