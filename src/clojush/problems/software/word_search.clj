;; word_search.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Problem Source: https://leetcode.com/problems/word-search/
;;

(ns clojush.problems.software.word-search
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

    ; Helper function 1 for input
    (defn puzzle-word-creation
      [row col direction puzzle]
      (cond
        (= direction 0) (string/join (rand-nth puzzle))
        :else (string/join (map #(nth % (rand-int col)) puzzle))))

; Helper function 2 for input
(defn make-word
  [row col puzzle use-puzzle]
  (if (= use-puzzle 1)
      (puzzle-word-creation row col (rand-int 2) puzzle)
      (apply str (repeatedly row #(char (+ 97 (rand-int 26)))))))

(defn make-row
  [len]
  (vec (map str (repeatedly len #(char (+ 97 (rand-int 26)))))))

; Atom generators
(def word-search-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec :vector_string :vector_vector_string])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def word-search-data-domains
  [[(list ["" [[]]]
          ["a" [["a"]]]
          ["b" [["a"]]]
          ["test" [["t" "e"]
                   ["s" "t"]]]
          ["test" [["t" "e" "s" "t"]]]
          ["nothere" [["a" "m" "v"]
                      ["k" "e" "l"]
                      ["a" "v" "i"]]]
          ["hello" [["s" "h" "e" "l" "l" "o" "p"]
                    ["e" "u" "a" "k" "v" "l" "d"]
                    ["q" "a" "p" "d" "k" "f" "l"]
                    ["g" "j" "e" "l" "a" "b" "z"]]]
          ["hello" [["a" "s" "d" "h" "p" "e" "o"]
                    ["v" "m" "n" "e" "l" "s" "k"]
                    ["q" "u" "e" "l" "i" "e" "o"]
                    ["v" "n" "c" "l" "q" "s" "u"]
                    ["d" "o" "u" "o" "z" "j" "b"]]]) 8 0] ;; "Special" inputs covering some base cases
   [(fn [] (let [row (inc (lrand-int 20))
                 col (inc (lrand-int 20))
                 puzzle (vec (repeatedly row #(make-row col)))
                 word (make-word row col puzzle (lrand-int 2))]
                 (vector word puzzle))) 192 2000]
   ])

;;Can make Word Search test data like this:
;(test-and-train-data-from-domains word-search-data-domains)

; Helper function for error function
(defn word-search-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[word puzzle]]
         (vector [word puzzle]
           (loop [current "" letter 0 row 0 col 0 direction "" start-row 0 same-letter false]
             (cond
               (= (apply str current) word) true   ; word has been found
               (and (= same-letter false)
                    (= (str (nth word letter)) (str (nth (nth puzzle row) col)))) (recur (concat current (nth (nth puzzle row) col)) (inc letter) row col direction start-row true)
               (and (>= row (dec (count puzzle)))
                    (>= col (dec (count (first puzzle))))) false   ; word was not found
               (and (= (str (nth word letter)) (str (nth (nth puzzle row) (inc col) nil))) ; letter to the right
                    (or (= direction "") (= direction "right"))) (recur current letter row (inc col) "right" start-row false)
               (and (= (str (nth word letter)) (str (nth (nth puzzle (inc row) nil) col))) ; letter down
                    (or (= direction "") (= direction "down"))) (recur current letter (inc row) col "down" start-row false)
               (= col (dec (count (first puzzle)))) (recur "" 0 (inc start-row) 0 "" (inc start-row) false) ; no letter, advance row, reset col
               :else (recur "" 0 start-row (inc col) "" start-row false))))) ; no letter, advance col
       inputs))

(defn make-word-search-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-word-search-error-function
    ([individual]
      (the-actual-word-search-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-word-search-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input1 input2] correct-output] (case data-cases
                                                                          :train train-cases
                                                                          :test test-cases
                                                                          [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input2 :input)
                                                     (push-item input1 :input)))
                             result (top-item :boolean final-state)]
                         (when print-outputs
                           (println (format "\n| Correct output: %b\n| Program output: %b" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is Levenshtein distance for printed string
                         (if (= result correct-output)
                           0
                           1))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-word-search-train-and-test
  "Returns the train and test cases."
  [data-domains]
    (map word-search-test-cases
        (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def word-search-train-and-test-cases
  (get-word-search-train-and-test word-search-data-domains))

(defn word-search-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first word-search-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second word-search-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn word-search-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Word Search problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-word-search-error-function-from-cases (first word-search-train-and-test-cases)
                                                             (second word-search-train-and-test-cases))
   :atom-generators word-search-atom-generators
   :max-points 4000
   :max-genome-size-in-initial-program 500
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report word-search-report
   :problem-specific-initial-report word-search-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
