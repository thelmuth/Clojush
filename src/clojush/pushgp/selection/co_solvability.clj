(ns clojush.pushgp.selection.co-solvability
  (:use [clojush util]))

(defn calculate-co-solvability-rewards-late
  "Late calculation of co-solbability rewards when normalization is
  set to min-max-scaling."
  [ind]
  (let [e (:errors ind)
        co-solve (for [x (range (count e))
                             y (range (count e))
                             :when (< x y)]
                         (/ (+ (- 1.0 (nth e x)) (- 1.0 (nth e y)))
                            2))]
    (assoc ind :co-solvability-rewards co-solve)))

(defn assign-co-solvability-error-to-individual
  "Takes an individual and calculates and assigns its co-solvability based on the summed
   error across each test case."
  [ind summed-reward-on-test-cases]
  (let [co-solvability-reward (apply +' (map #(if (zero? %2) 0.0 (/ %1 %2))
                                             (:co-solvability-rewards ind)
                                             summed-reward-on-test-cases))
        co-solvability-error (cond
                               (> co-solvability-reward 1e20) 0.0
                               (zero? co-solvability-reward) 1e20
                               (< 1e20 (/ 1.0 co-solvability-reward)) 1e20
                               :else (/ 1.0 co-solvability-reward))]
    (assoc ind :weighted-error co-solvability-error)))

(defn calculate-co-solvability
  "Calculates the summed fitness for each test case, and then uses it to
   assign an implicit fitness sharing error to each individual. Assumes
   :co-solvability-rewards has been calculated for each individual,
   and is a list of co-solvability rewards each in the range [0, 1]
   with higher being better and 1 being a solution."
  [pop-agents {:keys [use-single-thread]}]
  (println "\nCalculating co-solvability rewards...")
  (let [pop (map deref pop-agents)
        summed-reward-on-test-cases (map (fn [list-of-rewards]
                                           (reduce +' list-of-rewards))
                                         (apply map list
                                                (map :co-solvability-rewards pop)))]
    (println "First 10 co-solvability rewards per pair of training cases (lower means population performs worse):")
    (println (take 10 (map #(format "%.2f" %) summed-reward-on-test-cases)))
    (assert (every? (fn [reward] (< -0.0000001 reward 1.0000001))
                    (flatten (map :co-solvability-rewards pop)))
            (str "All rewards must be in range [0,1]. Please normalize them. Here are the first 20 offending errors:\n"
                 (not-lazy (take 20 (filter (fn [reward] (not (< 0.0 reward 1.0)))
                                            (flatten (map :co-solvability-rewards pop)))))))
    (dorun (map #((if use-single-thread swap! send)
                   %
                   assign-co-solvability-error-to-individual
                   summed-reward-on-test-cases)
                pop-agents))
    (when-not use-single-thread (apply await pop-agents)))) ;; SYNCHRONIZE

