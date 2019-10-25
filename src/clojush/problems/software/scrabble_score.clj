;; scrabble_score.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string of visible characters with length <= 20, return the Scrabble
;; score for that string. Each letter has a corresponding value according to
;; normal Scrabble rules, and non-letter character are worth zero.
;;
;; input stack has the input string

(ns clojush.problems.software.scrabble-score
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

(def scrabble-letter-values
  (let [scrabble-map {\a 1
                      \b 3
                      \c 3
                      \d 2
                      \e 1
                      \f 4
                      \g 2
                      \h 4
                      \i 1
                      \j 8
                      \k 5
                      \l 1
                      \m 3
                      \n 1
                      \o 1
                      \p 3
                      \q 10
                      \r 1
                      \s 1
                      \t 1
                      \u 1
                      \v 4
                      \w 4
                      \x 8
                      \y 4
                      \z 10}
        visible-chars (map char (range 0 127))]
    (vec (for [c visible-chars]
           (get scrabble-map (first (string/lower-case c)) 0)))))

; Atom generators
(def scrabble-score-atom-generators
  (concat (list
            scrabble-letter-values
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :vector_integer :exec])))

(defn make-scrabble-score-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-scrabble-score-error-function
    ([individual]
      (the-actual-scrabble-score-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-scrabble-score-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq
                       [[case-num [input1 correct-output]] (map-indexed vector (case data-cases
                                                                                 :train train-cases
                                                                                 :test test-cases
                                                                                 []))]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)))
                           result (stack-ref :integer 0 final-state)]



                       ; print if wrong answer
                       (when (or (not= (str result) correct-output))
                         (println "############################################################")
                         (println "Wrong result:" (pr-str input1) "||" correct-output result)
                         (println "############################################################"))
                                        ; print case numbers sometimes
                       (when (or (= (mod case-num 10000) 9999)
                                 (= (mod case-num 10000) 1))
                         (prn "At case" case-num ", input =", input1))  


                       
                       ))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def scrabble-score-train-and-test-cases
  (train-and-test-cases-from-dataset "scrabble-score" 0 9740000000))

(defn scrabble-score-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first scrabble-score-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second scrabble-score-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn scrabble-score-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Scrabble Score problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-scrabble-score-error-function-from-cases (first scrabble-score-train-and-test-cases)
                                                                  (second scrabble-score-train-and-test-cases))
   :atom-generators scrabble-score-atom-generators
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
   :problem-specific-report scrabble-score-report
   :problem-specific-initial-report scrabble-score-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000
   })



;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-max-points 4000)

(reset! global-evalpush-limit 2000)

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


;; This program is hand-written
(def tom-program
 '(
    0 in1 exec_string_iterate
    (
      [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 3 2 1 4 2 4 1 8 5 1 3 1 1 3 10 1 1 1 1 4 4 8 4 10 0 0 0 0 0 0 1 3 3 2 1 4 2 4 1 8 5 1 3 1 1 3 10 1 1 1 1 4 4 8 4 10 0 0 0 0]
      integer_fromchar vector_integer_nth
      integer_add
      )
    ))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
(run-push tom-program
          (push-item " \n " :input (make-push-state)))

;;; This makes sure the program works on all test and train cases:

(test-program-on-training tom-ind false)

(test-program-on-testing tom-ind false)




