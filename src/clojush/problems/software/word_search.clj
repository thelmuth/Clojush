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

; Creates a word that is guarenteed to be in the puzzle
(defn puzzle-word-creation
  [row col direction puzzle]
  (if (= direction 0) ; Word will be horizontal
      (let [start-subs (rand-int (quot row 2))
            end-subs (+ start-subs 1 (rand-int (quot row 2)))]
            (subs (rand-nth puzzle) start-subs end-subs))
      (let [start-subs (rand-int (quot col 2)) ; Word will be vertical
            end-subs (+ start-subs 1 (rand-int (quot col 2)))
            columns (apply mapv str puzzle)]
            (subs (rand-nth columns) start-subs end-subs))))

; Makes the word to be found. 25% will just create a random string, 75% will make sure word is in puzzle
(defn make-word
  [row col puzzle use-puzzle]
  (if (> use-puzzle 0)
      (puzzle-word-creation row col 1 puzzle)
      (apply str (repeatedly (inc (rand-int row)) #(char (+ 97 (rand-int 26)))))))

; Makes 1 row of a puzzle
(defn make-row
  [len]
  (apply str (repeatedly len #(char (+ 97 (rand-int 26))))))

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
          (registered-for-stacks [:string :char :integer :boolean :exec :vector_string :vector_integer])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def word-search-data-domains
  [[(list ["a" ["a"]]
          ["b" ["a"]]
          ["test" ["te"
                   "st"]]
          ["test" ["test"]]
          ["nothere" ["amv"
                      "kel"
                      "avi"]]
          ["hello" ["shellop"
                    "euakvld"
                    "qapdkfl"
                    "gjelabz"]]
          ["hello" ["napgzlo"
                    "asdhpeo"
                    "vmnelsk"
                    "quelieo"
                    "vnclqsu"
                    "douozjb"]]
          ["mid" ["adlvqpl"
                  "xdamoin"
                  "zqyizhm"
                  "hbpdbka"
                  "cqwzgon"]]) 8 0] ;; "Special" inputs covering some base cases
   [(fn [] (let [row (inc (rand-int 30))
                 col (inc (rand-int 30))
                 puzzle (vec (repeatedly row #(make-row col)))
                 word (make-word col row puzzle (rand-int 4))]
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
           (let [columns (apply mapv str puzzle)]
             (loop [current-row (first puzzle) current-col (first columns) row-num 0 col-num 0]
               (cond
                   (or (> (inc row-num) (count puzzle))
                       (> (inc col-num) (count columns))) ; Not in the puzzle
                     (vector -1 -1)
                   (clojure.string/includes? current-row word) ; In current-row
                     (vector row-num (clojure.string/index-of current-row word))
                   (clojure.string/includes? current-col word) ; In current-col
                     (vector (clojure.string/index-of current-col word) col-num)
                   :else (recur (nth puzzle (inc row-num) "") (nth columns (inc col-num) "") (inc row-num) (inc col-num)))))))
       inputs))

(define-registered
 output_integer1
 ^{:stack-types [:integer]}
 (fn [state]
   (if (empty? (:integer state))
     state
     (let [top-int (top-item :integer state)]
       (->> (pop-item :integer state)
            (stack-assoc top-int :output 0))))))


(define-registered
 output_integer2
 ^{:stack-types [:integer]}
 (fn [state]
   (if (empty? (:integer state))
     state
     (let [top-int (top-item :integer state)]
       (->> (pop-item :integer state)
            (stack-assoc top-int :output 1))))))

(defn make-word-search-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-word-search-error-function
    ([individual]
      (the-actual-word-search-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-word-search-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors
              (flatten
                (doall
                 (for [[[input1 input2] [correct-output1 correct-output2]] (case data-cases
                                                                            :train train-cases
                                                                            :test test-cases
                                                                            [])]
                   (let [final-state (run-push (:program individual)
                                               (->> (make-push-state)
                                                 (push-item :no-output :output)
                                                 (push-item :no-output :output)
                                                 (push-item input1 :input)
                                                 (push-item input2 :input)))
                         result1 (top-item :output final-state)
                         result2 (stack-ref :output 1 final-state)]
                     (when print-outputs
                       (println (format "\n| Correct output: %s %s\n| Program output: %s %s" (str correct-output1) (str correct-output2) (str result1) (str result2))))
                     ; Record the behavior
                     (swap! behavior conj result1 result2)
                     ; Error is integer differnce for row and column
                     (vector
                       (if (number? result1)
                           (abs (- result1 correct-output1)) ;distance from correct integer
                           100000) ;penalty for no return value
                       (if (number? result2)
                           (abs (- result2 correct-output2)) ;distance from correct integer
                           100000) ;penalty for no return value
                       )))))]
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
   :evalpush-limit 4000
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
   :error-threshold 0
   :max-error 100000
   })
