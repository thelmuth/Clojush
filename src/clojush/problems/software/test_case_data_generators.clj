(ns clojush.problems.software.test-case-data-generators
  (:use clojush.util)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojush.problems.software
             checksum
             collatz-numbers
             compare-string-lengths
             count-odds
             digits
             double-letters
             even-squares
             for-loop-index
             grade
             last-index-of-zero
             median
             mirror-image
             negative-to-zero
             number-io
             pig-latin
             replace-space-with-newline
             scrabble-score
             small-or-large
             smallest
             string-differences
             string-lengths-backwards
             sum-of-squares
             super-anagrams
             syllables
             vector-average
             vectors-summed
             wallis-pi
             word-stats
             x-word-lines
             ])
  (:import (java.util.zip ZipEntry ZipOutputStream))
  (:gen-class
    :name clojush.problems.software.tcdg
    :methods [#^{:static true} [generate_data_files [String int String String] String]]))

(def problem-map ;[multiple-inputs-bool data-domains test-case-generator]
  {"checksum" [false clojush.problems.software.checksum/checksum-data-domains clojush.problems.software.checksum/checksum-test-cases]
   "collatz-numbers" [false clojush.problems.software.collatz-numbers/collatz-numbers-data-domains clojush.problems.software.collatz-numbers/collatz-numbers-test-cases]
   "compare-string-lengths" [true clojush.problems.software.compare-string-lengths/csl-data-domains clojush.problems.software.compare-string-lengths/csl-test-cases]
   "count-odds" [false clojush.problems.software.count-odds/count-odds-data-domains clojush.problems.software.count-odds/count-odds-test-cases]
   "digits" [false clojush.problems.software.digits/digits-data-domains clojush.problems.software.digits/digits-test-cases]
   "double-letters" [false clojush.problems.software.double-letters/double-letters-data-domains clojush.problems.software.double-letters/double-letters-test-cases]
   "even-squares" [false clojush.problems.software.even-squares/even-squares-data-domains clojush.problems.software.even-squares/even-squares-test-cases]
   "for-loop-index" [true clojush.problems.software.for-loop-index/loop-data-domains clojush.problems.software.for-loop-index/loop-test-cases]
   "grade" [true clojush.problems.software.grade/grade-data-domains clojush.problems.software.grade/grade-test-cases]
   "last-index-of-zero" [false clojush.problems.software.last-index-of-zero/last-index-of-zero-data-domains clojush.problems.software.last-index-of-zero/last-index-of-zero-test-cases]
   "median" [true clojush.problems.software.median/median-data-domains clojush.problems.software.median/median-test-cases]
   "mirror-image" [true clojush.problems.software.mirror-image/mirror-image-data-domains clojush.problems.software.mirror-image/mirror-image-test-cases]
   "negative-to-zero" [false clojush.problems.software.negative-to-zero/negative-to-zero-data-domains clojush.problems.software.negative-to-zero/negative-to-zero-test-cases]
   "number-io" [true clojush.problems.software.number-io/num-io-data-domains clojush.problems.software.number-io/num-io-test-cases]
   "pig-latin" [false clojush.problems.software.pig-latin/pig-latin-data-domains clojush.problems.software.pig-latin/pig-latin-test-cases]
   "replace-space-with-newline" [false clojush.problems.software.replace-space-with-newline/replace-space-data-domains clojush.problems.software.replace-space-with-newline/replace-space-test-cases]
   "scrabble-score" [false clojush.problems.software.scrabble-score/scrabble-score-data-domains clojush.problems.software.scrabble-score/scrabble-score-test-cases]
   "small-or-large" [false clojush.problems.software.small-or-large/small-or-large-data-domains clojush.problems.software.small-or-large/small-or-large-test-cases]
   "smallest" [true clojush.problems.software.smallest/smallest-data-domains clojush.problems.software.smallest/smallest-test-cases]
   "string-differences" [true clojush.problems.software.string-differences/string-differences-data-domains clojush.problems.software.string-differences/string-differences-test-cases]
   "string-lengths-backwards" [false clojush.problems.software.string-lengths-backwards/string-lengths-data-domains clojush.problems.software.string-lengths-backwards/string-lengths-test-cases]
   "sum-of-squares" [false clojush.problems.software.sum-of-squares/sum-of-squares-data-domains clojush.problems.software.sum-of-squares/sum-of-squares-test-cases]
   "super-anagrams" [true clojush.problems.software.super-anagrams/super-anagrams-data-domains clojush.problems.software.super-anagrams/super-anagrams-test-cases]
   "syllables" [false clojush.problems.software.syllables/syllables-data-domains clojush.problems.software.syllables/syllables-test-cases]
   "vector-average" [false clojush.problems.software.vector-average/vector-average-data-domains clojush.problems.software.vector-average/vector-average-test-cases]
   "vectors-summed" [true clojush.problems.software.vectors-summed/vectors-summed-data-domains clojush.problems.software.vectors-summed/vectors-summed-test-cases]
   "wallis-pi" [false clojush.problems.software.wallis-pi/wallis-pi-data-domains clojush.problems.software.wallis-pi/wallis-pi-test-cases]
   "word-stats" [false clojush.problems.software.word-stats/word-stats-data-domains clojush.problems.software.word-stats/word-stats-test-cases]
   "x-word-lines" [true clojush.problems.software.x-word-lines/x-word-lines-data-domains clojush.problems.software.x-word-lines/x-word-lines-test-cases]
  })

(defn get-io-examples
  "Given a problem, returns vector of [training examples, testing examples]."
  [problem]
  (let [[multiple-inputs-bool data-domains test-case-generator] (get problem-map problem)]
    (map test-case-generator (test-and-train-data-from-domains data-domains))))

(defn get-writeable-data
  "Given a software problem, returns a vector of vectors, where each vector is
  a single data point, including inputs and outputs. First vector is a header of
  the form:
     [set, input1, input2, ..., output1, output2, ...]"
  [problem]
  (let [[train test] (get-io-examples problem)
        [first-input first-output] (first train)
        multiple-inputs (first (get problem-map problem))
        number-inputs (count (if multiple-inputs
                               first-input
                               (list first-input)))
        number-outputs (count (cond
                                (= problem "replace-space-with-newline") first-output
                                (= problem "even-squares") (list (first first-output))
                                (= problem "word-stats") (list (first first-output))
                                :else (list first-output)))
        header [(concat ["set"]
                         (map #(str "input" %)
                              (range 1 (inc number-inputs)))
                         (map #(str "output" %)
                              (range 1 (inc number-outputs))))]
        train-data (map (fn [[input output]]
                          (conj (concat (if multiple-inputs
                                          input
                                          (list input))
                                        (cond
                                          (= problem "replace-space-with-newline") output
                                          (= problem "even-squares") (list (first output))
                                          (= problem "word-stats") (list (first output))
                                          :else (list output)))
                                "train"))
                        train)
        test-data (map (fn [[input output]]
                         (conj (concat (if multiple-inputs
                                         input
                                         (list input))
                                       (cond
                                         (= problem "replace-space-with-newline") output
                                         (= problem "even-squares") (list (first output))
                                         (= problem "word-stats") (list (first output))
                                         :else (list output)))
                               "test"))
                       test)]
    (concat header train-data test-data)))

(defn write-data-to-csv
  "Takes given data as a vector of vectors, where each internal vector
  is a line to write in the CSV. Then, writes the CSV file."
  [data-to-write csv-filename]
  (with-open [csv-file (io/writer csv-filename :append false)]
    (csv/write-csv csv-file data-to-write)))

(defn generate-and-write-data-to-file
  "Generates train and test data for the given problem. Then, writes that data
  to the given file, using the given file structure"
  [problem output-filename file-type]
  (let [data-to-write (get-writeable-data problem)]
    (case file-type
      "csv" (write-data-to-csv data-to-write output-filename)
      (throw (Exception. (str "Unrecognized file type: " file-type))))))

(defn generate-data-files
  "Generates data for given problem, in a number of files specified by the argument.
  Puts all generated files into a zip. Returns the location of the generated zip file."
  [problem number-of-files file-type path]
  (let [directory (apply str (repeatedly 8 #(rand-nth "abcdefghijklmnopqrstuvwxyz")))
        prefix (str path problem "-" directory)
        suffix (case file-type
                 "csv" ".csv"
                 (throw (Exception. (str "Unrecognized file type: " file-type))))
        ]
    (.mkdir (java.io.File. prefix))
    (doseq [index (range number-of-files)]
      (generate-and-write-data-to-file problem
                                       (str prefix
                                            "/"
                                            problem
                                            index
                                            suffix)
                                       file-type))
    (with-open [zip (ZipOutputStream. (io/output-stream (str prefix ".zip")))]
      (doseq [f (file-seq (io/file prefix)) :when (.isFile f)]
        (.putNextEntry zip (ZipEntry. (.getName f)))
        (io/copy f zip)
        (.closeEntry zip)))
    (doseq [index (range number-of-files)]
      (io/delete-file (str prefix
                           "/"
                           problem
                           index
                           suffix)))
    (io/delete-file prefix)
    (str prefix ".zip")))

(defn -generate_data_files
  "Java method for this function"
  [problem number-of-files file-type path]
  (generate-data-files problem number-of-files file-type path))

;(generate-data-files "last-index-of-zero" 7 "csv" "/Users/helmuth/Desktop/generated-data/")

