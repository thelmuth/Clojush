;; for_loop_index.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given 3 integer inputs (start, finish, stepSize), print the integers
;; represented by the Java for loop:
;;      for(i = start; i < finish; i += stepSize) System.out.println(i);
;;
;; Note that start < finish for all test cases, so will always require printing something.
;;
;; Note: tried adding extra error, did not help
;;
;; input stack has 3 input integers: in1 = start, in2 = finish, in3 = step-size

(ns clojush.problems.software.for-loop-index
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def loop-atom-generators
  (concat (list
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :print])))

(defn make-for-loop-index-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-loop-error-function
    ([individual]
      (the-actual-loop-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-loop-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq
                       [[case-num [input1 input2 input3 correct-output]] (map-indexed vector (case data-cases
                                                                                               :train train-cases
                                                                                               :test test-cases
                                                                                               []))]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input3 :input)
                                                      (push-item input2 :input)
                                                      (push-item input1 :input)
                                                      (push-item "" :output)))
                           result (stack-ref :output 0 final-state)]
                       (when print-outputs
                         (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                                        ; Record the behavior


                                        ; print if wrong answer
                       (when (not= result correct-output)
                         (println "############################################################")
                         (println "Wrong result:" input1 correct-output result)
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
(def for-loop-index-train-and-test-cases
  (train-and-test-cases-from-dataset "for-loop-index" 0 10000000000))

(defn for-loop-index-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first for-loop-index-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second for-loop-index-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn loop-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- For Loop Index problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-for-loop-index-error-function-from-cases (first for-loop-index-train-and-test-cases)
                                                                  (second for-loop-index-train-and-test-cases))
   :atom-generators loop-atom-generators
   :max-points 1200
   :max-genome-size-in-initial-program 150
   :evalpush-limit 600
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
   :problem-specific-report loop-report
   :problem-specific-initial-report for-loop-index-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })


;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-evalpush-limit 600)

(reset! global-max-points 1200)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
(def tom-program
  '(in1 integer_dup print_integer exec_y
        (in3 integer_add exec_stackdepth integer_dup_times
             exec_rot () (print_newline print_integer)
             (integer_inc in2 integer_gt exec_if))))

;; This program is hand-written
#_(def tom-program
 '(
    4 in1 integer_lt
    exec_when
    (
      4 print_integer
      4 integer_dup integer_dup integer_mult integer_dup in1 integer_lt
      exec_while
      (
        print_newline print_integer 
        integer_inc integer_inc
        integer_dup integer_dup integer_mult integer_dup in1 integer_lt
        )
      )
    ))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind  false)

;(test-program-on-testing tom-ind false)


