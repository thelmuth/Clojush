(ns clojush.pushgp.selection.pareto-tournament
  (:use [clojush random util]))

(defn dominates?
  "Returns true if indA dominates indB on objectives, False otherwise, including
  if indA and indB are equal on objectives."
  [indA indB objectives]
  (if (= (map #(get indA %) objectives)
         (map #(get indB %) objectives))
    false
    (loop [obs objectives]
      (cond
        (empty? obs) ; if here, indB is not better on any objective than indA, and is not equal to indA, so indB is dominated by indA
        true

        (< (get indB (first obs)) (get indA (first obs))) ; if here, indB is better on an objective, so it is not dominated by indA
        false

        ; recur on next objective
        :else
        (recur (rest obs))))))

(defn non-dominated?
  "Returns true if the individual is non-dominated compared to population
  on the given objectives. Objectives must be keys to the individuals. An ind
  equal to another in the population is non-dominated if not dominated by another
  individual."
  [ind population objectives]
  (empty? (filter #(dominates? % ind objectives) population)))

(defn pareto-tournament-selection
  "Returns a random individual that is on the pareto front of the
  individuals in the tournament. Objectives are given as
  pareto-tournament-objectives. Options currently are:
   :total-error
   :size
   :age"
  [pop {:keys [tournament-size pareto-tournament-objectives]}]
  (let [tournament-set (repeatedly tournament-size #(lrand-nth pop))
        tournament-set-with-size (if (some #{:size} pareto-tournament-objectives)
                                   (map (fn [ind]
                                          (assoc ind :size
                                                 (count-points (:program ind))))
                                        tournament-set)
                                   tournament-set)
        pareto-front (filter #(non-dominated? % tournament-set-with-size pareto-tournament-objectives)
                             tournament-set-with-size)]
    (lrand-nth pareto-front)))
