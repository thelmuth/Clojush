(ns clojush.pushgp.selection.lexicase
  (:use [clojush random])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn shuffle-cases
  [pop argmap]
  (if (= (:sort-meta-errors-for-lexicase argmap) :random)
    (lshuffle (range (count (:errors (first pop)))))
    (let [num-all-errors (count (:errors (first pop))) ;; will included meta-errors, added in select
          num-meta-errors (count (:meta-errors (first pop)))
          num-true-errors (- num-all-errors num-meta-errors)
          true-error-indices (range num-true-errors)
          meta-error-indices (map #(+ % num-true-errors)
                                  (range num-meta-errors))]
      (case (:sort-meta-errors-for-lexicase argmap)
        :first (concat (lshuffle meta-error-indices)
                       (lshuffle true-error-indices))
        :last (concat (lshuffle true-error-indices)
                      (lshuffle meta-error-indices))))))

(defn lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
  time in random order."
  [pop location {:keys [generation csv-case-usage-filename csv-individuals-remaining] :as argmap} parent-number]
  (loop [survivors pop
         cases (shuffle-cases pop argmap)
         step 1
         individuals-remaining [[generation location parent-number 0 -1 (count survivors)]]]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (let [selected-ind (lrand-nth survivors)
            cases-used (- (count (:errors (first pop)))
                          (count cases))]
        ;; Write CSV for case usage
        (with-open [csv-file (io/writer csv-case-usage-filename :append true)]
          (csv/write-csv csv-file
                         [[generation
                            ;(str (:uuid selected-ind)) ; Not needed, since rank and generation uniquely identify each individual
                            cases-used
                            (:rank-by-total-error selected-ind)]]))
        ;; Write CSV for number of individuals remaining after every step
        (with-open [csv-file (io/writer csv-individuals-remaining :append true)]
          (csv/write-csv csv-file
                         individuals-remaining))
        selected-ind)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))
            new-survivors (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                                  survivors)]
        (recur new-survivors
               (rest cases)
               (inc step)
               (conj individuals-remaining
                     [generation
                      location
                      parent-number
                      step
                      (first cases)
                      (count new-survivors)]))))))
