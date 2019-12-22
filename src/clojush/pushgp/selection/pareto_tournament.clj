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

(defn distinct-in-objective-space
  "Returns a sequence of distinct individuals in objective space.
  So, if two individuals have the same values for each key in
  objectives, only one with be in the resulting list."
  [inds objectives]
  (map rand-nth
       (vals (group-by (fn [ind] (map #(get ind %) objectives))
                       inds))))

#_(distinct-in-objective-space
 '({:a 5 :b 3 :c :A}
   {:a 2 :b 3 :c :B}
   {:a 5 :b 3 :c :C}
   {:a 5 :b 1 :c :D}
   {:a 5 :b 1 :c :E}
   {:a 5 :b 1 :c :F}
   {:a 22 :b 22 :c G})
 [:a :b])

(defn pareto-tournament-queue-manager
  "Called on pareto-tournament-queue to return a new parent and manage the queue"
  [pt-queue population {:keys [tournament-size pareto-tournament-objectives]}]
  (if (not (empty? (:queue pt-queue)))
                                        ; If not empty, put first ind in queue as new parent and pop out of queue
    {:new-parent (first (:queue pt-queue))
     :queue (rest (:queue pt-queue))}
                                        ; If empty, find pareto front of a new tournament
    (let [tournament-set (repeatedly tournament-size #(lrand-nth population))
          tournament-set-with-size (distinct-in-objective-space
                                    (if (some #{:size} pareto-tournament-objectives)
                                      (map (fn [ind]
                                             (assoc ind :size
                                                    (count-points (:program ind))))
                                           tournament-set)
                                      tournament-set)
                                    pareto-tournament-objectives)
          pareto-front (filter #(non-dominated? % tournament-set-with-size pareto-tournament-objectives)
                               tournament-set-with-size)]
      (println (map :age pareto-front))
      
      {:new-parent (first pareto-front)
       :queue (rest pareto-front)})))


(def pareto-tournament-queue (atom {:new-parent nil
                                    :queue '()}))

(defn pareto-tournament-selection
  "Uses pareto-tournaments to select individuals to be parents.
  If pareto-tournament-queue is empty, conducts a pareto tournament and
  fills it. Then, or if it is not empty, the first individual in the queue
  is selected to be a parent.
  Objectives are given as  pareto-tournament-objectives. Options currently are:
   :total-error
   :size
   :age"
  [population argmap]
  (:new-parent (swap! pareto-tournament-queue pareto-tournament-queue-manager population argmap)))
