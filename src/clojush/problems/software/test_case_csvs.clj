;; ********** NOTE **********
;; I haven't figured out how to handle vector inputs yet. Right now, I'm printing
;; clojure-readable strings for vectors in CSVs, and cannot write the vectors
;; to xlsx files.

(ns clojush.problems.software.test-case-csvs
  (:use [clojush.util]
        dk.ative.docjure.spreadsheet)
  (:require [clojush.problems.software
             checksum
             collatz-numbers
             compare-string-lengths
;             count-odds
             digits
             double-letters
             even-squares
             for-loop-index
             grade
;             last-index-of-zero
             median
;             mirror-image
;             negative-to-zero
             number-io
             pig-latin
             replace-space-with-newline
             scrabble-score
             small-or-large
             smallest
             string-differences
;             string-lengths-backwards
             sum-of-squares
             super-anagrams
             syllables
;             vector-average
;             vectors-summed
             wallis-pi
             word-stats
             x-word-lines
             ]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))



(def problems-domains-and-test-cases-generators
  [; [problem-string multiple-inputs-bool data-domains test-case-generator]
   ["checksum" false clojush.problems.software.checksum/checksum-data-domains clojush.problems.software.checksum/checksum-test-cases]
;   ["collatz-numbers" false clojush.problems.software.collatz-numbers/collatz-numbers-data-domains clojush.problems.software.collatz-numbers/collatz-numbers-test-cases]
;   ["compare-string-lengths" true clojush.problems.software.compare-string-lengths/csl-data-domains clojush.problems.software.compare-string-lengths/csl-test-cases]
;   ["count-odds" false clojush.problems.software.count-odds/count-odds-data-domains clojush.problems.software.count-odds/count-odds-test-cases]
;   ["digits" false clojush.problems.software.digits/digits-data-domains clojush.problems.software.digits/digits-test-cases]
;   ["double-letters" false clojush.problems.software.double-letters/double-letters-data-domains clojush.problems.software.double-letters/double-letters-test-cases]
;   ["even-squares" false clojush.problems.software.even-squares/even-squares-data-domains clojush.problems.software.even-squares/even-squares-test-cases]
;   ["for-loop-index" true clojush.problems.software.for-loop-index/loop-data-domains clojush.problems.software.for-loop-index/loop-test-cases]
;   ["grade" true clojush.problems.software.grade/grade-data-domains clojush.problems.software.grade/grade-test-cases]
;   ["last-index-of-zero" false clojush.problems.software.last-index-of-zero/last-index-of-zero-data-domains clojush.problems.software.last-index-of-zero/last-index-of-zero-test-cases]
;   ["median" true clojush.problems.software.median/median-data-domains clojush.problems.software.median/median-test-cases]
;   ["mirror-image" true clojush.problems.software.mirror-image/mirror-image-data-domains clojush.problems.software.mirror-image/mirror-image-test-cases]
;   ["negative-to-zero" false clojush.problems.software.negative-to-zero/negative-to-zero-data-domains clojush.problems.software.negative-to-zero/negative-to-zero-test-cases]
;   ["number-io" true clojush.problems.software.number-io/num-io-data-domains clojush.problems.software.number-io/num-io-test-cases]
;   ["pig-latin" false clojush.problems.software.pig-latin/pig-latin-data-domains clojush.problems.software.pig-latin/pig-latin-test-cases]
;   ["replace-space-with-newline" false clojush.problems.software.replace-space-with-newline/replace-space-data-domains clojush.problems.software.replace-space-with-newline/replace-space-test-cases]
;   ["scrabble-score" false clojush.problems.software.scrabble-score/scrabble-score-data-domains clojush.problems.software.scrabble-score/scrabble-score-test-cases]
;   ["small-or-large" false clojush.problems.software.small-or-large/small-or-large-data-domains clojush.problems.software.small-or-large/small-or-large-test-cases]
;   ["smallest" true clojush.problems.software.smallest/smallest-data-domains clojush.problems.software.smallest/smallest-test-cases]
;   ["string-differences" true clojush.problems.software.string-differences/string-differences-data-domains clojush.problems.software.string-differences/string-differences-test-cases]
;   ["string-lengths-backwards" false clojush.problems.software.string-lengths-backwards/string-lengths-data-domains clojush.problems.software.string-lengths-backwards/string-lengths-test-cases]
;   ["sum-of-squares" false clojush.problems.software.sum-of-squares/sum-of-squares-data-domains clojush.problems.software.sum-of-squares/sum-of-squares-test-cases]
;   ["super-anagrams" true clojush.problems.software.super-anagrams/super-anagrams-data-domains clojush.problems.software.super-anagrams/super-anagrams-test-cases]
;   ["syllables" false clojush.problems.software.syllables/syllables-data-domains clojush.problems.software.syllables/syllables-test-cases]
;   ["vector-average" false clojush.problems.software.vector-average/vector-average-data-domains clojush.problems.software.vector-average/vector-average-test-cases]
;   ["vectors-summed" true clojush.problems.software.vectors-summed/vectors-summed-data-domains clojush.problems.software.vectors-summed/vectors-summed-test-cases]
;   ["wallis-pi" false clojush.problems.software.wallis-pi/wallis-pi-data-domains clojush.problems.software.wallis-pi/wallis-pi-test-cases]
;   ["word-stats" false clojush.problems.software.word-stats/word-stats-data-domains clojush.problems.software.word-stats/word-stats-test-cases]
;   ["x-word-lines" true clojush.problems.software.x-word-lines/x-word-lines-data-domains clojush.problems.software.x-word-lines/x-word-lines-test-cases]
   ])

(defn get-io-examples
  "Returns vector of training examples and testing examples."
  [data-domains test-case-generator]
  (let [[train test] (map test-case-generator (test-and-train-data-from-domains data-domains))]
    [train test]))

(defn get-printable-data
  [[train test] multiple-inputs problem]
  (let [[first-input first-output] (first train)
        number-inputs (count (if multiple-inputs
                               first-input
                               (list first-input)))
        number-outputs (count (cond
                                (= problem "replace-space-with-newline") first-output
                                (= problem "even-squares") (list (first first-output))
                                (= problem "word-stats") (list (first first-output))
                                :else (list first-output)))
        train-header [(concat (map #(str "train_input_" %)
                                            (range 1 (inc number-inputs)))
                                       (map #(str "train_output_" %)
                                            (range 1 (inc number-outputs))))]
        test-header [(concat (map #(str "test_input_" %)
                                            (range 1 (inc number-inputs)))
                                       (map #(str "test_output_" %)
                                            (range 1 (inc number-outputs))))]
        train-data (map (fn [[input output]]
                            (concat (if multiple-inputs
                                      input
                                      (list input))
                                    (cond
                                      (= problem "replace-space-with-newline") output
                                      (= problem "even-squares") (list (first output))
                                      (= problem "word-stats") (list (first output))
                                      :else (list output))))
                          train)
        test-data (map (fn [[input output]]
                            (concat (if multiple-inputs
                                      input
                                      (list input))
                                    (cond
                                      (= problem "replace-space-with-newline") output
                                      (= problem "even-squares") (list (first output))
                                      (= problem "word-stats") (list (first output))
                                      :else (list output))))
                          test)]
    (concat train-header train-data test-header test-data)))

(defn print-examples-to-csv
  [[train test] multiple-inputs problem filename-prefix print-csv print-xls]
  (let [csv-log-filename (str filename-prefix problem ".csv")
        xls-filename (str filename-prefix problem ".xlsx")
        all-lines (get-printable-data [train test] multiple-inputs problem)]
    (when print-csv
      (with-open [csv-file (io/writer csv-log-filename :append false)]
        (csv/write-csv csv-file all-lines)))
    (when print-xls
      (save-workbook! xls-filename (create-workbook problem all-lines)))))

(defn print-csvs-for-problems
  [problems-with-domains-and-tests filename-prefix print-csv print-xls]
  (doseq [[problem multiple-inputs domains test-generators] problems-with-domains-and-tests]
    (println problem)
      (print-examples-to-csv (get-io-examples domains test-generators)
                             multiple-inputs
                             problem
                             filename-prefix
                             print-csv
                             print-xls)))

(print-csvs-for-problems problems-domains-and-test-cases-generators
                         "DELETE-LATER-"
                         true
                         false)

; Fake argmap
(def argmap
  {:error-function (fn [p] [0])
   :atom-generators (list 0)
   :max-points 10
   :max-genome-size-in-initial-program 2
   :population-size 1
   :final-report-simplifications 0
   })
