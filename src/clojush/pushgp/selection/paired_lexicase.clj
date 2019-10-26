(ns clojush.pushgp.selection.paired-lexicase
  (:use [clojush random]
        clojush.pushgp.selection.lexicase))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paired lexicase selection

(defn paired-lexicase-selection
  "Lexicase selection, except considered on pairs (or triples, ..., or n-tuples). First, partition
   the test cases into pairs. Then, run lexicase, but at each step keep around anyone that is elite
   on at least one of the tests in the pair (or n-tuple). Thus 1-tuple paired lexicase is equivalent
   to vanilla lexicase."
  [pop {:keys [paired-lexicase-tuple-size] :as argmap}]
  (loop [survivors pop
         case-tuples (partition paired-lexicase-tuple-size
                               (shuffle-cases pop argmap))]
    (if (or (empty? case-tuples)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-errors-for-cases (map (fn [case]
                                        (apply min (map #(nth % case)
                                                        (map #(:errors %) survivors))))
                                      (first case-tuples))]
        (recur (filter (fn [ind]
                         (some identity ; This has the same functionality as (apply or ...), which doesn't work since or is a macro
                               (map (fn [case min-error-for-case]
                                      (= (nth (:errors ind) case)
                                         min-error-for-case))
                                    (first case-tuples)
                                    min-errors-for-cases)))
                       survivors)
               (rest case-tuples))))))
