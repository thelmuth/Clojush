(ns clojush.pushgp.selection.divide-and-conquer-lexicase
  (:use [clojush random]))

(defn best-in-group-on-case
  "Takes a group of individuals, usually 2 (but could be 1, and could be more in a variant),
   as a list. Also takes a test case index. Returns the individual in the group that's best
   on the test case. If none is best, returns a random out of the best."
  [group case]
  (let [min-error-for-case-in-group (apply min (map #(nth (:errors %) case)
                                                    group))]
    (lrand-nth (filter #(= (nth (:errors %) case)
                           min-error-for-case-in-group)
                       group))))

(defn divide-and-conquer-lexicase-selection
  "Pairs off population into groups of size grouped-individuals.
   Then, keeps only the one with better error
   on the next test case. Repeats process, removing half the population at each step,
   until down to one individual, the parent. Note: We shuffle the population at each
   step to: 1) Make sure the same individuals aren't always paired together, and
   2) Since the last individual in an odd-lengthed survivors list always makes it
   through, this makes it so it isn't the same guy each time."
  [population {:keys [divide-and-conquer-lexicase-group-size]}]
  (loop [survivors (lshuffle population)
         cases (lshuffle (range (count (:errors (first population)))))]
    (if (or (empty? cases) ;This won't happen unless (length cases) < (log_2 survivors)
            (empty? (rest survivors)))
      (first survivors)
      (let [grouped-individuals (partition divide-and-conquer-lexicase-group-size
                                           divide-and-conquer-lexicase-group-size
                                           []
                                           survivors)
            new-survivors (map #(best-in-group-on-case % (first cases))
                               grouped-individuals)]
        (recur (lshuffle new-survivors)
               (rest cases))))))
