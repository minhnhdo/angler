(ns angler.errors)

(defmacro checks
  "Usage: (checks [check-and-failure-result*] success-result)

  check-and-failure-result => pred failure-result"
  [checks-and-failure-results success-result]
  (let [rev-checks (rseq (vec (partition 2 checks-and-failure-results)))]
    (reduce (fn [r [pred failure-result]]
              (list 'if pred r failure-result))
            success-result
            rev-checks)))

(defn parse-error
  [message]
  {:error ::parse-error
   :message message})

(defn read-error
  [message]
  {:error ::read-error
   :message message})
