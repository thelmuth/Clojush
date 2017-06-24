(ns clojush.problems.software.test-case-data-generators
  (:use clojush.util)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojush.problems.software
             median
             replace-space-with-newline])
  (:import (java.util.zip ZipEntry ZipOutputStream))
  (:gen-class
    :name clojush.problems.software.tcdg
    :methods [#^{:static true} [generate_data_files [String int String] String]]))

(def problem-map ;[multiple-inputs-bool data-domains test-case-generator]
  {"median" [true clojush.problems.software.median/median-data-domains clojush.problems.software.median/median-test-cases]
   "replace-space-with-newline" [false clojush.problems.software.replace-space-with-newline/replace-space-data-domains clojush.problems.software.replace-space-with-newline/replace-space-test-cases]
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
  [problem number-of-files file-type]
  (let [directory (apply str (repeatedly 8 #(rand-nth "abcdefghijklmnopqrstuvwxyz")))
        prefix (str "generated-data/" problem "-" directory)
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
        (.putNextEntry zip (ZipEntry. (.getPath f)))
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
  [problem number-of-files file-type]
  (generate-data-files problem number-of-files file-type))

;(generate-data-files "median" 5 "csv")

