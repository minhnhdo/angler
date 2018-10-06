(ns angler.autodiff
  (:require [angler.types :refer [built-ins]]))

(def ^:private supported-operations
  {'+ {:fn (get built-ins '+)
       :derivatives [(fn [a b] 1) (fn [a b] 1)]}
   '- {:fn (get built-ins '-)
       :derivatives [(fn [a b] 1) (fn [a b] -1)]}
   '* {:fn (get built-ins '*)
       :derivatives [(fn [a b] b) (fn [a b] a)]}
   '/ {:fn (get built-ins '/)
       :derivatives [(fn [a b] (/ 1 b)) (fn [a b] (* a (/ -1 (* b b))))]}
   'exp {:fn (get built-ins 'exp)
         :derivatives [(fn [a] ((get built-ins 'exp) a))]}
   'log {:fn (get built-ins 'log)
         :derivatives [(fn [a] (/ 1 a))]}
   'relu {:fn (fn [a] (max 0 a))
          :derivatives [(fn [a] (if (> a 0) 1 0))]}
   'sin {:fn (get built-ins 'sin)
         :derivatives [(fn [a] ((get built-ins 'cos) a))]}
   'cos {:fn (get built-ins 'cos)
         :derivatives [(fn [a] (- ((get built-ins 'sin) a)))]}
   'normpdf {:fn (fn [x mu sigma]
                   (let [x-mu (- x mu)
                         two-variance (* 2 sigma sigma)]
                     (* (/ 1 ((get built-ins 'sqrt) (* Math/PI two-variance)))
                        ((get built-ins 'exp) (- (/ (* x-mu x-mu)
                                                    two-variance))))))
             :derivatives [(fn [x mu sigma]
                             (let [mu-x (- mu x)
                                   variance (* sigma sigma)]
                               (/ (* mu-x
                                     ((get built-ins 'exp) (- (/ (* mu-x mu-x)
                                                                 (* 2 variance)))))
                                  (* ((get built-ins 'sqrt) (* 2 Math/PI))
                                     variance
                                     sigma))))
                           (fn [x mu sigma]
                             (let [mu-x (- mu x)
                                   variance (* sigma sigma)]
                               (/ (* (- mu-x)
                                     ((get built-ins 'exp) (- (/ (* mu-x mu-x)
                                                                 (* 2 variance)))))
                                  (* ((get built-ins 'sqrt) (* 2 Math/PI))
                                     variance
                                     sigma))))
                           (fn [x mu sigma]
                             (let [mu-x (- mu x)
                                   variance (* sigma sigma)]
                               (- (/ (* (- sigma mu-x)
                                        (+ sigma mu-x)
                                        ((get built-ins 'exp) (/ (* mu-x mu-x)
                                                                 (* 2 variance))))
                                  (* ((get built-ins 'sqrt) (* 2 Math/PI))
                                     variance
                                     variance)))))]}
   '= {:fn (get built-ins '=)}
   '> {:fn (get built-ins '>)}
   '< {:fn (get built-ins '<)}
   '>= {:fn (get built-ins '>=)}
   '<= {:fn (get built-ins '<=)}
   'not {:fn (get built-ins 'not)}})

(defn- autodiff-forward
  [sub exp]
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
                   :primal (apply (get-in supported-operations [fn-name :fn])
                                  real-args)})
    (symbol? exp) {:is-arg? true
                   :arg exp
                   :primal (get sub exp)}
    :else {:is-const? true
           :primal exp}))

(defn autodiff
  [f args]
  (let [[_ params body] f
        sub (apply hash-map (interleave params args))]
    (println (autodiff-forward sub body))))
