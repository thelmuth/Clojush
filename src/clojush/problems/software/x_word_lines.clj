;; x_word_lines.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given an integer 1 <= X <= 10 and a string of at most 100 characters that likely
;; contains spaces and newlines, print the string with exactly X words per line.
;; The last line may have fewer than X words.
;;
;; This version uses 3 error metrics on each training case
;;
;; input stack has the input string and integer

(ns clojush.problems.software.x-word-lines
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

; Atom generators
(def x-word-lines-atom-generators
  (concat (list
            \space
            \newline
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))

(defn make-x-word-lines-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-x-word-lines-error-function
    ([individual]
      (the-actual-x-word-lines-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-x-word-lines-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                     (doall
                       (for [[[input1 input2] correct-output] (case data-cases
                                                                :train train-cases
                                                                :test test-cases
                                                                [])]
                         (let [final-state (run-push (:program individual)
                                                     (->> (make-push-state)
                                                       (push-item input2 :input)
                                                       (push-item input1 :input)
                                                       (push-item "" :output)))
                               result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                           ; Record the behavior
                           (swap! behavior conj result)
                           (vector
                             ; First error is Levenshtein distance of printed strings
                             (levenshtein-distance correct-output result)
                             ; Second error is integer distance from the correct number of newlines
                             (abs (- (count (filter #(= % \newline) correct-output))
                                     (count (filter #(= % \newline) result))))
                             ; Third error is summed error of integer distances over the lines of the correct number of words per line
                             (+ (apply + (map #(abs (- input2
                                                       (count (string/split (string/trim %) #"\s+"))))
                                              (butlast (string/split-lines result))))
                                (abs (- (count (string/split (string/trim (last (string/split-lines correct-output))) #"\s+"))
                                        (count (string/split (string/trim (let [last-line (last (string/split-lines result))]
                                                                            (if last-line last-line "")))
                                                             #"\s+")))))
                             )))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def x-word-lines-train-and-test-cases
  (train-and-test-cases-from-dataset "x-word-lines" 104 2000))

(defn x-word-lines-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first x-word-lines-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second x-word-lines-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn x-word-lines-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- X-Word Lines problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-x-word-lines-error-function-from-cases (first x-word-lines-train-and-test-cases)
                                                                (second x-word-lines-train-and-test-cases))
   :atom-generators x-word-lines-atom-generators
   :max-points 3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit 1600
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
   :problem-specific-report x-word-lines-report
   :problem-specific-initial-report x-word-lines-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
