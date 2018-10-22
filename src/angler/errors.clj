(ns angler.errors)

(gen-class
  :name angler.errors.CompileError
  :extends RuntimeException
  :constructors {[String] [String] [] []}
  :main false)

(defmacro checks
  "Usage: (checks [check-and-failure-result*] success-result)

  check-and-failure-result => pred failure-result"
  [checks-and-failure-results success-result]
  (let [rev-checks (rseq (vec (partition 2 checks-and-failure-results)))]
    (reduce (fn [r [pred failure-result]]
              (list 'if pred r failure-result))
            success-result
            rev-checks)))

(defmacro checked->
  "Usage: (checked-> expr check-and-operate*)

  check-and-operate => check operate

  Example:
  (checked->
    1
    (> 0) (+ 1)
    even? (+ 1))
  will expand to
  (let [a 1]
    (if (> a 1)
      (let [a (+ a 1)]
        (if (even? a)
          (let [a (+ a 1)]
            a)
          a))
      a))"
  [expr & checks-and-operates]
  (let [fresh (gensym)
        rev-checks (rseq (vec checks-and-operates))]
    (list 'let [fresh expr]
          (nth
            (reduce (fn [[is-let r] op]
                      (let [action (if (and (list? op) (not (nil? (first op))))
                                     (apply list (first op) fresh (rest op))
                                     (list op fresh))]
                        (if is-let
                          [false (list 'let [fresh action] r)]
                          [true (list 'if action r fresh)])))
                    [true fresh]
                    rev-checks)
            1))))

(defn check-error
  [r]
  (not (:angler.errors/error r)))

(defn debug
  [x]
  (println x)
  x)

(defn compile-error
  [& args]
  {::error ::compile-error
   ::message (apply str args)})

(defn graph-error
  [& args]
  {::error ::graph-error
   ::message (apply str args)})

(defn infer-error
  [& args]
  {::error ::infer-error
   ::message (apply str args)})

(defn read-error
  [& args]
  {::error ::read-error
   ::message (apply str args)})

(defn scope-error
  [& args]
  {::error ::scope-error
   ::message (apply str args)})

(defn validate-error
  [& args]
  {::error ::validate-error
   ::message (apply str args)})
