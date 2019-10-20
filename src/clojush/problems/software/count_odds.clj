;; count_odds.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers with length <= 50, with each
;; integer in [-1000,1000], return the number of integers that are odd.
;;
;; input stack has 1 input vector of integers

(ns clojush.problems.software.count-odds
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def count-odds-atom-generators
  (concat (list
            0
            1
            2
            ;;; end constants
            (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :vector_integer :exec])))

(defn make-count-odds-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-count-odds-error-function
    ([individual]
      (the-actual-count-odds-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-count-odds-error-function individual data-cases false))
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
                           result (top-item :integer final-state)]
                       (when print-outputs
                         (println (format "Correct output: %2d | Program output: %s" correct-output (str result))))

                       ; print if wrong answer
                       (when (not= result correct-output)
                         (println "Wrong result:" input1 correct-output result))
                       ; print case numbers sometimes
                       (when (or (= (mod case-num 10000) 9999)
                                 (= (mod case-num 10000) 1))
                         (println "At case" case-num))  
                       
                                        ; Error is integer error

                                        ;
                       
                       ))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(def count-odds-train-and-test-cases
  (train-and-test-cases-from-dataset "count-odds" 0 20000000000))

(defn count-odds-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first count-odds-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second count-odds-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn count-odds-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Count Odds problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-count-odds-error-function-from-cases (first count-odds-train-and-test-cases)
                                                              (second count-odds-train-and-test-cases))
   :atom-generators count-odds-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 1500
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
   :problem-specific-report count-odds-report
   :problem-specific-initial-report count-odds-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000
   })




;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-evalpush-limit 1500)

(reset! global-max-points 2000)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
(def tom-program
 '(in1 1 vector_integer_conj exec_do*vector_integer
       (2 integer_mod)
       exec_do*count exec_eq integer_min integer_gt exec_do*while
       (integer_add vector_integer_empty)))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind false)

(test-program-on-testing tom-ind false)



