(ns clojush.pushgp.selection.sampled-lexicase-tournament
  (:use [clojush random globals util]
        clojush.pushgp.selection.preselection))

(defn return-index-for-samples-lexicase-selection
  "Conducts one run of lexicase selection on population with added indices, and
   returns the index of the winning individual."
  [pop]
  (loop [survivors (one-individual-per-error-vector-for-lexicase pop {:parent-selection :sampled-lexicase-tournament})
         cases (lshuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (:index (lrand-nth survivors))
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

(defn sample-for-lexicase-tournament-selection
  "Samples population using lexicase selection, counting numbers of selections
   per individual. Number of samples is twice population size.
   Creates global atom vector lexicase-tournament-samples, which contains maps
   of the form {:lexicase-samples n :individual i}.
   Options:
     - lexicase-tournament-remove-zero-sample-individuals - if true, individuals that receive 0 samples will not participate in tournaments; if false, they will."
  [pop-agents {:keys [population-size
                      lexicase-tournament-remove-zero-sample-individuals]}]
  (println "\nCalculate samples for lexicase-tournament selection...")
  (let [pop-with-samples (vec (map (fn [ind] {:individual ind :lexicase-samples 0})
                                   (map deref pop-agents)))
        pop-with-indices (vec (map (fn [ind index] (assoc ind :index index))
                                   (map deref pop-agents)
                                   (range)))
        number-samples (* 2 population-size)
        the-sampled-indices (pmap (fn [_] (return-index-for-samples-lexicase-selection pop-with-indices))
                                  (range number-samples))
        result-with-sample-counts (vec (reduce (fn [cur-pop index]
                                             (update-in cur-pop [index :lexicase-samples] inc))
                                           pop-with-samples
                                           the-sampled-indices))]
    (println "Lexicase Sample Counts (sorted):" (sort > (map :lexicase-samples result-with-sample-counts)))
    (println "Programs With One Or More Lexicase Samples:" (count (remove #(zero? (:lexicase-samples %)) result-with-sample-counts)))
    (reset! lexicase-tournament-samples (if lexicase-tournament-remove-zero-sample-individuals
                                          (remove #(zero? (:lexicase-samples %)) result-with-sample-counts)
                                          result-with-sample-counts))))

(defn sampled-lexicase-tournament-selection
  "Uses repeated lexicase selection samples to rank the population by how often
   lexicase selects each individual. Then, uses those rankings in tournaments
   to select parents.
   Options:
     - tournament-size - size for the lexicase tournaments"
  [{:keys [tournament-size]}]
  (let [tournament-set (doall (for [_ (range tournament-size)]
                                (lrand-nth @lexicase-tournament-samples)))
        winning-ind-map (reduce (fn [i1 i2]
                                  (if (> (:lexicase-samples i1) (:lexicase-samples i2))
                                    i1
                                    i2))
                                tournament-set)]
    (:individual winning-ind-map)))
