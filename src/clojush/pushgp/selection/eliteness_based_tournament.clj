(ns clojush.pushgp.selection.eliteness-based-tournament
  (:use [clojush random]))

(defn eliteness-based-tournament-selection
  "Tournament selection, where the fitness of an individual is the number of test cases
   on which the individual has the best error value of any individual in the tournament."
  [pop {:keys [tournament-size]}]
  (let [tournament-set (repeatedly tournament-size
                                  (fn []
                                    (lrand-nth pop)))
        min-error-on-each-case (map (partial apply min)
                                    (apply mapv vector ; transpose error vectors into vector of errors per case
                                           (map :errors tournament-set)))]
    (:ind (apply max-key ; Find the individual with maximum number of elite cases
                 :elite-cases
                 (map (fn [ind]
                        {:ind ind
                         :elite-cases (count ; The nunmber of cases on which individual's error = min error
                                        (filter identity
                                                (map = min-error-on-each-case (:errors ind))))})
                      tournament-set)))))

