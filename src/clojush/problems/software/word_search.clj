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
    (= direction 0) (take col (drop (+ row col) puzzle))
    :else (loop [current col word ""]
                (cond
                  (< current (* row col)) (recur (+ current col) (str word (nth puzzle current)))
                  :else word))))

; Helper function 2 for input
(defn make-word
  [row col puzzle use-puzzle]
  (if (= use-puzzle 1)
      (puzzle-word-creation row col (lrand-int 2) puzzle)
      (apply str (repeatedly row #(char (+ 97 (lrand-int 26)))))))

;; Define test cases
(defn word-search-input
  "Makes a word search input of size row col."
  [row col]
  (let [puzzle (apply str (repeatedly (* row col) #(char (+ 97 (lrand-int 26)))))
        word (apply str (make-word row col puzzle (lrand-int 2)))]
        (vector word puzzle row col)))

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
            'in3
            'in4
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def word-search-data-domains
  [[(list ["" "" 0 0]
          ["a" "a" 1 1]
          ["b" "a" 1 1]
          ["test" "test" 2 2]
          ["test" "test" 1 4]
          ["nothere" "amvkelavi" 3 3]
          ["hello" "shellopeuakvldqapdkflgjelabz" 4 7]
          ["hello" "asdhpeovmnelskquelieovnclqsudouozjb" 5 7]) 8 0] ;; "Special" inputs covering some base cases
   [(fn [] (let [row (inc (lrand-int 20)) col (inc (lrand-int 20))]
                (word-search-input row col))) 192 2000]
   ])

;;Can make Word Search test data like this:
;(test-and-train-data-from-domains word-search-data-domains)

; Helper function for error function
(defn word-search-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[word puzzle row col]]
         (vector [word puzzle row col]
           (loop [current "" letter 0 pos 0 startpos 0 samepos false direction ""]
             (cond
               (= current word) true ; if the word is found, return true
               (and
                    (= startpos (dec (* row col)))
                    (not= current word)) false   ; if the word isn't in the puzzle, return false
               (and (= samepos false)
                    (= (nth word letter) (nth puzzle pos))) (recur (str current (nth puzzle pos)) (inc letter) pos startpos true direction) ; if the current letter is correct
               (and (not= (mod (inc pos) col) 0)  ; if the next letter to the right is correct and it isn't out of bounds
                    (= (nth word letter) (nth puzzle (inc pos)))
                    (or (= direction "") (= direction "right"))) (recur current letter (inc pos) startpos false "right")
               (and (< (+ pos col) (* row col))  ; if the next letter is down and it isn't out of bounds
                    (= (nth word letter) (nth puzzle (+ pos col)))
                    (or (= direction "") (= direction "down"))) (recur current letter (+ pos col) startpos false "down")
               :else (recur "" 0 (inc startpos) (inc startpos) false "")))))
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
                     (for [[[input1 input2 input3 input4] correct-output] (case data-cases
                                                                          :train train-cases
                                                                          :test test-cases
                                                                          [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input4 :input)
                                                     (push-item input3 :input)
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
