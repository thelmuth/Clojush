(ns clojush.pushgp.weighted-lexicase
  (:use [clojush random globals util]))


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
  (let [sqr (fn [x] (*' x x))
        mean-vect (/ (reduce +' vector-of-error) (count vector-of-error))]
    (/
      (reduce +'
              (map #(sqr (-' % mean-vect)) vector-of-error))
      (count vector-of-error))))

(defn variance-inverse
  [vector-of-error]
  (if (= (variance vector-of-error) 0)
    200
    (/ 1 (variance vector-of-error))))

(defn non-zero
  [vector-of-error]
  (- (count vector-of-error) (count (filter zero? vector-of-error))))

(defn median
  [vector-of-error]
  (let [sorted (sort vector-of-error) 
        counted (count vector-of-error)
        bottom (dec (quot counted 2))]
    (if (odd? counted)
      (nth sorted (quot counted 2))
      (/ (+ (nth sorted (quot counted 2)) (nth sorted bottom)) 2))))

(defn median-inverse
  [vector-of-error]
  (if (= 0 (median vector-of-error))
    2
    (/ 1 (median vector-of-error))))

(defn number-zero-inverse
  [vector-of-error]
  (if (= (count (filter zero? vector-of-error)) 0)
    2
    (/ 1 (count (filter zero? vector-of-error)))))

(defn non-zero-inverse
  [vector-of-error]
  (if (= (count vector-of-error) (count (filter zero? vector-of-error)))
    2
    (/ 1 (- (count vector-of-error) (count (filter zero? vector-of-error))))))

    
      



(defn calculate-test-case-weights
  [pop-agents {:keys [weighted-lexicase-bias]}]
  (let [test-case-error-vectors (apply map vector (map :errors (map deref pop-agents))) 
        bias-function (case weighted-lexicase-bias
                        :number-of-zeros (fn [vector-of-errors] (count(filter zero? vector-of-errors)))
                        :average (fn [vector-of-errors] (/ (reduce + vector-of-errors)(count vector-of-errors)))
                        :number-of-nonzeros (fn [vector-of-errors] (non-zero vector-of-errors))
                        :number-of-nonzeros-inverse (fn [vector-of-error] (non-zero-inverse vector-of-error))
                        :variance (fn [vector-of-errors](variance vector-of-errors))
                        :variance-inverse (fn [vector-of-error] (variance-inverse vector-of-error))
                        :number-of-zeros-inverse (fn [vector-of-errors] (number-zero-inverse vector-of-errors))
                        :median (fn [vector-of-errors] (median vector-of-errors))
                        :median-inverse (fn [vector-of-error] (median-inverse vector-of-error)))]
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
