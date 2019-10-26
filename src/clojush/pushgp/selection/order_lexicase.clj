(ns clojush.pushgp.selection.order-lexicase
  (:use [clojush random]
        clojush.pushgp.selection.lexicase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; order lexicase selection

(defn order-lexicase-selection
  "Identical to lexicase selection, except that instead of only keeping elite
   individuals at each step, keeps the best order-lexicase-retention-rate pecent
   individuals. If there is a tie at the boundry, keeps all individuals that tie.
   For example, if order-lexicase-retention-rate == 0.5, then at each step we
   keep any individual with error at least as good as the median individual on
   that test case."
  [pop {:keys [order-lexicase-retention-rate] :as argmap}]
  (assert (and (<= 0 order-lexicase-retention-rate)
               (<= order-lexicase-retention-rate 1))
          ":order-lexicase-retention-rate must be in the range [0.0 1.0]")
  (loop [survivors pop
         cases (shuffle-cases pop argmap)]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [k (dec (Math/round (* (count survivors)
                             order-lexicase-retention-rate)))
            kth-order-statistic (if (< k 0)
                                  0
                                  k)
            kth-err-for-case (nth (sort (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))
                                  kth-order-statistic)
            better-than-kth (filter #(< (nth (:errors %) (first cases)) kth-err-for-case)
                                    survivors)
            new-survivors (if (not (empty? better-than-kth))
                            better-than-kth
                            (filter #(= (nth (:errors %) (first cases)) kth-err-for-case) ;If there are no better errors than the kth error, that means the kth error is equal to the best error. Keep all errors equal to best error.
                                    survivors))]
        (recur new-survivors
               (rest cases))))))
