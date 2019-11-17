(ns clojush.pushgp.selection.batch-tournament
  (:use [clojush random]))

(defn batch-tournament-selection
  "Implements batch-tournament selection (specifically BTSS) from the paper
    'Batch Tournament Selection for Genetic Programming'
  Makes some adjustments that make implementation much easier, but should have
  same effect. In particular, each batch is just randomly selected
  during selection, instead of partitioning the cases in a shuffled order."
  [pop {:keys [batch-tournament-batch-size tournament-size] :as argmap}]
  (let [batch-cases (take batch-tournament-batch-size
                          (lshuffle (range (count (:errors (first pop))))))
        tournament-set (repeatedly tournament-size #(lrand-nth pop))
        ts-with-batch-errors (map (fn [ind]
                                    (assoc ind
                                           :batch-total-error
                                           (apply +'
                                                  (map #(nth (:errors ind) %)
                                                       batch-cases))))
                                  tournament-set)]
    (dissoc (apply min-key :batch-total-error ts-with-batch-errors)
            :batch-total-error)))
