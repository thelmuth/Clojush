;; string_differences.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given 2 strings of length 10 or less (without whitespace) as input, find the indices at which
;; the strings have different chraracters, stopping at the end of the shorter
;; one. For each such index, print a line containing the index as well as the
;; character in each string. For example, if the strings are "dealer" and
;; "dollars", your code should report this:
;;    1 e o
;;    2 a l
;;    4 e a
;;
;; input stack has the 2 input strings
;;
;; NOTE: I could add a second error value per case. For example, comparing the
;; number of newlines to the correct number of newlines. Or that the format
;; of each line is correct (number, space, char, space, char). Or something similar.
;;
;; NOTE 2: I have added an error for the correct format. This problem is still too
;; difficult. It could possibly be made easier by guaranteeing that the two
;; strings are of the same length.

(ns clojush.problems.software.string-differences
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

; Atom generators
(def string-differences-atom-generators
  (concat (list
            \space
            \newline
            ;;; end constants
            (fn [] (- (lrand-int 21) 10)) ;; Integer ERC [-10,10]
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :string :char :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))

(defn make-string-differences-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-string-differences-error-function
    ([individual]
      (the-actual-string-differences-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-string-differences-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq [[case-num [input1 input2 correct-output]] (map-indexed vector (case data-cases
                                                                                           :train train-cases
                                                                                           :test test-cases
                                                                                           []))]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input2 :input)
                                                      (push-item input1 :input)
                                                      (push-item "" :output)))
                           result (stack-ref :output 0 final-state)]

                       

                                        ; print if wrong answer
                       (when (not= result correct-output)
                         (println "############################################################")
                         (println "Wrong result:" input1 "||" correct-output result)
                         (println "############################################################"))
                                        ; print case numbers sometimes
                       (when (or (= (mod case-num 10000) 9999)
                                 (= (mod case-num 10000) 1))
                         (prn "At case" case-num ", input =", input1))  
                       

                       ))] ;;NOTE: SEE NOTE IN INTRO
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def string-differences-train-and-test-cases
  (train-and-test-cases-from-dataset "string-differences" 0 20000000))

(defn string-differences-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first string-differences-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second string-differences-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn string-differences-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- String Differences problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-string-differences-error-function-from-cases (first string-differences-train-and-test-cases)
                                                                      (second string-differences-train-and-test-cases))
   :atom-generators string-differences-atom-generators
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
   :problem-specific-report string-differences-report
   :problem-specific-initial-report string-differences-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })


;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-max-points 2000)

(reset! global-evalpush-limit 1500)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
#_(def tom-program
  '(boolean_stackdepth in1 vector_integer_dup exec_do*vector_integer
                       (integer_min exec_do*times boolean_stackdepth
                                    vector_integer_replace vector_integer_stackdepth)))

#_(def tom-program-BAD
  '(integer_stackdepth integer_stackdepth exec_do*range
                       (exec_do*vector_integer in1) integer_dup 42 exec_do*times
                       (integer_mod integer_swap) integer_inc exec_do*times integer_lte
                       vector_integer_dup_times integer_stackdepth))

;; This program is hand-written
(def tom-program
  '(
     in1 string_length in2 string_length integer_min ;get length of shorter string
     integer_dup 0 integer_lte exec_when exec_flush ;when shorter string has length <= 0, don't do anything
     exec_do*count
     (
       integer_dup integer_dup
       in1 string_nth in2 string_nth
       char_eq boolean_not
       exec_when
       (
         boolean_empty boolean_not exec_when
         print_newline
         true ; put on boolean stack just to know when have been here before
         integer_dup integer_dup integer_dup
         print_integer
         \space print_char
         in1 string_nth print_char
         \space print_char
         in2 string_nth print_char
         )
       )
     ))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind false)

(test-program-on-testing tom-ind false)


