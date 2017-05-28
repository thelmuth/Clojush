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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;weighted lexicase selection

(defn weighted-shuffle
  []
  (loop [map-of-weighted-cases @testcase-weights
         shuffled-case-list []]
    (if (empty? map-of-weighted-cases)
      shuffled-case-list
      (let [total (reduce + (vals map-of-weighted-cases))
            randnum (lrand total)
            test-cases-with-endpoints (reductions (fn [[cur-ind cur-sum]
                                                       [new-ind new-sum]]
                                                    [new-ind (+ cur-sum new-sum)])
                                                  map-of-weighted-cases)
            chosen-test-case (first (first (filter (fn [[test-case-number endpoint]]
                                                     (<= randnum endpoint))
                                                   test-cases-with-endpoints)))]
        (if (zero? total)
          (concat shuffled-case-list (shuffle (keys map-of-weighted-cases)))
          (recur (dissoc map-of-weighted-cases chosen-test-case)
                 (conj shuffled-case-list chosen-test-case)))))))

(defn variance 
  [vector-of-error]
  (def sqr (fn [x] (*' x x)))
  (let [mean-vect (/ (reduce +' vector-of-error)(count vector-of-error))]
    (/
      (reduce +'
              (map #(sqr (-' % mean-vect)) vector-of-error))
      (count vector-of-error))))

(defn variance-inverse
  [vector-of-error]
  (if (= (variance vector-of-error)0)
    200
    (/ 1 (variance vector-of-error))))

(defn non-zero
  [vector-of-error]
  (- (count vector-of-error)(count(filter zero? vector-of-error))))

(defn median
  [vector-of-error]
  (let [sorted (sort vector-of-error) 
        counted (count vector-of-error)
        bottom (dec (quot counted 2))]
    (if (odd? counted)
      (nth sorted (quot counted 2))
      (/ (+(nth sorted (quot counted 2))(nth sorted bottom))2))))

(defn median-inverse
  [vector-of-error]
  (if (= 0 (median vector-of-error))
    2
    (/ 1 (median vector-of-error))))

(defn number-zero-inverse
  [vector-of-error]
  (if (= (count(filter zero? vector-of-error))0)
    2
    (/ 1 (count(filter zero? vector-of-error)))))

(defn non-zero-inverse
  [vector-of-error]
  (if (= (count vector-of-error)(count(filter zero? vector-of-error)))
    2
    (/ 1 (- (count vector-of-error)(count(filter zero? vector-of-error))))))

    
      



(defn calculate-test-case-weights
  [pop-agents {:keys [weighted-lexicase-bias]}]
  (let [test-case-error-vectors (apply map vector (map :errors (map deref pop-agents))) 
        bias-function (case weighted-lexicase-bias
                        :number-of-zeros (fn [vector-of-errors] (count(filter zero? vector-of-errors)))
                        :sum (fn[vector-of-errors] (reduce + vector-of-errors))
                        :average (fn [vector-of-errors](/ (reduce + vector-of-errors)(count vector-of-errors))
                                   )
                        :number-of-nonzero (fn [vector-of-errors] (non-zero vector-of-errors))
                        :number-of-nonzero-inverse (fn [vector-of-error] (non-zero-inverse vector-of-error))
                        :variance (fn [vector-of-errors](variance vector-of-errors))
                        :variance-inverse (fn [vector-of-error] (variance-inverse vector-of-error))
                        :number-of-zeros-inverse (fn [vector-of-errors] (number-zero-inverse vector-of-errors))
                        :median (fn [vector-of-errors] (median vector-of-errors))
                        :median-inverse (fn [vector-of-error] (median-inverse vector-of-error))
                        )]
    (reset! testcase-weights (into {} (map vector 
                                           (range)
                                           (map bias-function test-case-error-vectors))))))



        
(defn weighted-lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a time in a random biased order 
with more weight given to certain cases by some metric.
If trivial-geography-radius is non-zero, selection is limited to parents within +/- r of location"
  [pop location {:keys [trivial-geography-radius]}]
  ;(println @testcase-weights)
  ;(println (weighted-shuffle))
  (loop [survivors (retain-one-individual-per-error-vector pop)
         cases (weighted-shuffle)]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;biased lexicase selection


(defn rank-cases
  "This takes a the @testcase-weights and returns a list of tast case ordered by rank
   NOTE: Old version broke ties deterministically, meaning that tied test cases would always
         be ranked in the same order, which may have made it worse. Now ties broken randomly"
  []
  (loop [map-of-weighted-cases @testcase-weights
         ranked-list []]
    (if (empty? map-of-weighted-cases)
      ranked-list
      (let [highest-weight (val (apply max-key val map-of-weighted-cases))
            chosen-test-case (key (lrand-nth (filter #(= (val %) highest-weight) map-of-weighted-cases)))]
        (recur (dissoc map-of-weighted-cases chosen-test-case)
               (conj ranked-list chosen-test-case))))))


(defn bias-lexicase-selection
 [pop location {:keys [tournament-size trivial-geography-radius]}]
 (let [tournament-set 
       (doall
         (for [_ (range tournament-size)]
           (nth pop
                (if (zero? trivial-geography-radius)
                  (lrand-int (count pop))
                  (mod (+ location (- (lrand-int (+ 1 (* trivial-geography-radius 2))) trivial-geography-radius))
                       (count pop))))))]
   
   (loop [survivors (retain-one-individual-per-error-vector tournament-set)
        cases (rank-cases)]
   (if (or (empty? cases)
           (empty? (rest survivors)))
     (lrand-nth survivors)
     (let [min-err-for-case (apply min (map #(nth % (first cases))
                                            (map #(:errors %) survivors)))]
       (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                      survivors)
              (rest cases)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;ranked lexicase selection

(defn bias-ordering-of-cases-based-on-rank
  "Takes list of test cases ranked by some metric, and returns a biased shuffled
   list of cases, earlier-ranked cases have higher probability of coming sooner
   in the shuffled ordering."
  []
  (let [ranked-cases (rank-cases)]
    (loop [result []
           remaining-cases (vec ranked-cases)
           number-cases-remaining (count ranked-cases)]
      (if (empty? remaining-cases) 
        result
        (let [upper-bound (lrand-int number-cases-remaining)
              index (lrand-int (inc upper-bound))]
          (recur (conj result (nth remaining-cases index))
                 (vec (concat (subvec remaining-cases 0 index)
                              (subvec remaining-cases (inc index))))
                 (dec number-cases-remaining)))))))

(defn ranked-lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
   time in a biased order determined by a rank of some metric.  If trivial-geography-radius is non-zero, selection is limited to parents within +/- r of location"
  [pop location {:keys [trivial-geography-radius]}]
  
 
  (loop [survivors (retain-one-individual-per-error-vector pop)
         cases (bias-ordering-of-cases-based-on-rank)]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))



;(defn bias-ordering-of-cases-based-on-rank-probabilities-of-selection
;  "Calculates the exact probability of each rank being selected out of n ranked
;   cases with bias ordering of test cases above."
;  [n]
;  (map (fn [i]
;         (float (/ (apply + (map #(/ 1 %)
;                                 (range (inc i) (inc n))))
;                   n)))
  


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
                   :weighted-lexicase (weighted-lexicase-selection pop-with-meta-errors location argmap)
                   :bias-lexicase (bias-lexicase-selection pop-with-meta-errors location argmap)
                   :ranked-lexicase (ranked-lexicase-selection pop-with-meta-errors location argmap)
                   :uniform (uniform-selection pop-with-meta-errors)
                   (throw (Exception. (str "Unrecognized argument for parent-selection: "
                                           parent-selection))))]
    (when print-selection-counts
      (swap! selection-counts update-in [(:uuid selected)] (fn [sel-count]
                                                             (if (nil? sel-count)
                                                               1
                                                               (inc sel-count)))))
    selected))
