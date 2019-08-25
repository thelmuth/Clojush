(ns clojush.pushgp.selection.subset-tournament
  (:use [clojush random])
  (:require [incanter.stats :as incstats]))

(defn subset-tournament-selection
  "Returns an individual that does the best out of a tournament using only a subset
  of the error cases."
  [pop {:keys [tournament-size total-error-method] :as argmap}]
  (let [tournament-set (doall (for [_ (range tournament-size)]
                                (lrand-nth pop)))
        total-cases (count (:errors (first pop)))
        all-cases (range total-cases)
        cases (if (< (rand) 0.8)
                (take (inc (rand-int (dec total-cases)))
                      (shuffle all-cases))
                (if (< (rand 0.5))
                  [(rand-nth all-cases)]
                  all-cases))
        err-fn (fn [ind]
                 (apply + 
                        (for [c cases]
                            (nth (:errors ind) c))))]
    (apply min-key err-fn tournament-set)))

(defn subset-tournament-selection-with-normal-case-distribution
  "Returns an individual that does the best out of a tournament using only a subset
  of the error cases.
  Percent of cases to use is based on random sampling of a
  normal distribution with defualt mu = 0.2, sigma = 0.1, limited
  to the range [1, total-cases]"
  [pop {:keys [tournament-size total-error-method subset-tournament-selection-mu subset-tournament-selection-sigma] :as argmap}]
  (let [tournament-set (if (>= tournament-size (count pop))
                         pop
                         (doall (for [_ (range tournament-size)]
                                  (lrand-nth pop))))
        total-cases (count (:errors (first pop)))
        percent-cases-to-use (incstats/sample-normal 1 :mean subset-tournament-selection-mu :sd subset-tournament-selection-sigma)
        num-cases-to-use (max 1 (min total-cases (Math/round (* percent-cases-to-use total-cases))))
        all-cases (range total-cases)
        cases (take num-cases-to-use
                    (shuffle all-cases))
        err-fn (fn [ind]
                 (apply +
                        (for [c cases]
                            (nth (:errors ind) c))))]
    (apply min-key err-fn tournament-set)))
