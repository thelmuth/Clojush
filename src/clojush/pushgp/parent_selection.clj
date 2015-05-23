(ns clojush.pushgp.parent-selection
  (:use [clojush random globals util])
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tournament selection
(defn tournament-selection
  "Returns an individual that does the best out of a tournament."
  [pop location {:keys [tournament-size trivial-geography-radius
                        total-error-method]}]
  (let [tournament-set 
        (doall
          (for [_ (range tournament-size)]
            (nth pop
                 (if (zero? trivial-geography-radius)
                   (lrand-int (count pop))
                   (mod (+ location (- (lrand-int (+ 1 (* trivial-geography-radius 2))) trivial-geography-radius))
                        (count pop))))))
        err-fn (case total-error-method
                 :sum :total-error
                 (:hah :rmse :ifs) :weighted-error
                 (throw (Exception. (str "Unrecognized argument for total-error-method: "
                                         total-error-method))))]
    (reduce (fn [i1 i2] (if (< (err-fn i1) (err-fn i2)) i1 i2))
            tournament-set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexicase selection

(defn retain-one-individual-per-error-vector
  "Retains one random individual to represent each error vector."
  [pop]
  (map lrand-nth (vals (group-by #(:errors %) pop))))
  
(defn lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
   time in random order.  If trivial-geography-radius is non-zero, selection is limited to parents within +/- r of location"
  [pop location {:keys [trivial-geography-radius]}]
  (let [lower (mod (- location trivial-geography-radius) (count pop))
        upper (mod (+ location trivial-geography-radius) (count pop))
        popvec (vec pop)
        subpop (if (zero? trivial-geography-radius) 
                 pop
                 (if (< lower upper)
                   (subvec popvec lower (inc upper))
                   (into (subvec popvec lower (count pop)) 
                         (subvec popvec 0 (inc upper)))))]
    (loop [survivors (retain-one-individual-per-error-vector subpop)
           cases (lshuffle (range (count (:errors (first subpop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (lrand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map #(:errors %) survivors)))]
          (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                         survivors)
                 (rest cases)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elitegroup lexicase selection

(defn build-elitegroups
  "Builds a sequence that partitions the cases into sub-sequences, with cases 
   grouped when they produce the same set of elite individuals in the population. 
   In addition, if group A produces population subset PS(A), and group B 
   produces population subset PS(B), and PS(A) is a proper subset of PS(B), then 
   group B is discarded. "
  [pop-agents]
  (println "Building case elitegroups...")
  (let [pop (retain-one-individual-per-error-vector (map deref pop-agents))
        cases (range (count (:errors (first pop))))
        elites (map (fn [c]
                      (let [min-error-for-case 
                            (apply min (map #(nth % c) (map :errors pop)))]
                        (filter #(== (nth (:errors %) c) min-error-for-case)
                                pop)))
                    cases)
        all-elitegroups (vals (group-by #(nth elites %) cases))
        pruned-elitegroups (filter (fn [eg]
                                     (let [e (set (nth elites (first eg)))]
                                       (not-any?
                                         (fn [eg2]
                                           (let [e2 (set (nth elites (first eg2)))]
                                             (and (not= e e2)
                                                  (set/subset? e2 e))))
                                         all-elitegroups)))
                                   all-elitegroups)]
    (reset! elitegroups pruned-elitegroups)
    (println (count @elitegroups) "elitegroups:" @elitegroups)))

(defn elitegroup-lexicase-selection
  "Returns an individual produced by elitegroup lexicase selection."
  [pop]
  (loop [survivors (retain-one-individual-per-error-vector pop)
         cases (lshuffle (map lrand-nth @elitegroups))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implicit fitness sharing

(defn assign-ifs-error-to-individual
  "Takes an individual and calculates and assigns its IFS based on the summed
   error across each test case."
  [ind summed-reward-on-test-cases]
  (let [ifs-reward (apply +' (map #(if (zero? %2) 1.0 (/ %1 %2))
                                  (map #(- 1.0 %) (:errors ind))
                                  summed-reward-on-test-cases))
        ifs-er (cond
                 (< 1e20 ifs-reward) 0.0
                 (zero? ifs-reward) 1e20
                 (< 1e20 (/ 1.0 ifs-reward)) 1e20
                 :else (/ 1.0 ifs-reward))]
    (assoc ind :weighted-error ifs-er)))

(defn calculate-implicit-fitness-sharing
  "Calculates the summed fitness for each test case, and then uses it to
   assign an implicit fitness sharing error to each individual. Assumes errors
   are in range [0,1] with 0 being a solution."
  [pop-agents {:keys [use-single-thread]}]
  (println "\nCalculating implicit fitness sharing errors...")
  (let [pop (map deref pop-agents)
        summed-reward-on-test-cases (map (fn [list-of-errors]
                                           (reduce +' (map #(- 1.0 %) list-of-errors)))
                                         (apply map list (map :errors pop)))]
    (println "Implicit fitness sharing reward per test case (lower means population performs worse):")
    (println summed-reward-on-test-cases)
    (assert (every? (fn [error] (< -0.0000001 error 1.0000001))
                    (flatten (map :errors pop)))
            (str "All errors must be in range [0,1]. Please normalize them. Here are the first 20 offending errors:\n"
                 (not-lazy (take 20 (filter (fn [error] (not (< 0.0 error 1.0)))
                                            (flatten (map :errors pop)))))))
    (dorun (map #((if use-single-thread swap! send)
                   %
                   assign-ifs-error-to-individual
                   summed-reward-on-test-cases)
                pop-agents))
    (when-not use-single-thread (apply await pop-agents)))) ;; SYNCHRONIZE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexicase tournament selection

(defn return-index-for-samples-lexicase-selection
  "Conducts one run of lexicase selection on population with added indices, and
   returns the index of the winning individual."
  [pop]
  (loop [survivors (retain-one-individual-per-error-vector pop)
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
    (println "Programs With One Or More Lexicase Samples:" (count (remove #(zero? %)
                                                                          (map :lexicase-samples result-with-sample-counts))))
    (reset! lexicase-tournament-samples (if lexicase-tournament-remove-zero-sample-individuals
                                          (remove #(zero? (:lexicase-samples %)) result-with-sample-counts)
                                          result-with-sample-counts))))

(defn lexicase-tournament-selection
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniform selection (i.e. no selection, for use as a baseline)

(defn uniform-selection
  "Returns an individual uniformly at random."
  [pop]
  (lrand-nth pop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parent selection

(defn select
  "Returns a selected parent."
  [pop location {:keys [parent-selection print-selection-counts]
                 :as argmap}]
  (let [pop-with-meta-errors (map (fn [ind] (update-in ind [:errors] concat (:meta-errors ind)))
                                  pop)
        selected (case parent-selection
                   :tournament (tournament-selection pop-with-meta-errors location argmap)
                   :lexicase (lexicase-selection pop-with-meta-errors location argmap)
                   :elitegroup-lexicase (elitegroup-lexicase-selection pop-with-meta-errors)
                   :leaky-lexicase (if (< (lrand) (:lexicase-leakage argmap))
                                     (uniform-selection pop-with-meta-errors)
                                     (lexicase-selection pop-with-meta-errors location argmap))
                   :lexicase-tournament (lexicase-tournament-selection argmap)
                   :uniform (uniform-selection pop-with-meta-errors)
                   (throw (Exception. (str "Unrecognized argument for parent-selection: "
                                           parent-selection))))]
    (when print-selection-counts
      (swap! selection-counts update-in [(:uuid selected)] (fn [sel-count]
                                                             (if (nil? sel-count)
                                                               1
                                                               (inc sel-count)))))
    selected))
