;; median.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given 3 integers, print their median.
;;
;; input stack has the 3 integers

(ns clojush.problems.software.median
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def median-atom-generators
  (concat (list
            (fn [] (- (lrand-int 201) 100))
            (tag-instruction-erc [:exec :integer :boolean] 1000)
            (tagged-instruction-erc 1000)
            ;;; end ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :print])))

(defn make-median-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-median-error-function
    ([individual]
      (the-actual-median-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-median-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq
                       [[case-num [input1 input2 input3 correct-output]] (map-indexed vector
                                                                               (case data-cases
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


                       
                                                               ; print if wrong answer
                       (when (not= result (str correct-output))
                         (println "############################################################")
                         (println "Wrong result:" input1 input2 input3 "||" correct-output result)
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
(def median-train-and-test-cases
  (train-and-test-cases-from-dataset "median" 0 10000000))

(defn median-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first median-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second median-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn median-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Median problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-median-error-function-from-cases (first median-train-and-test-cases)
                                                          (second median-train-and-test-cases))
   :atom-generators median-atom-generators
   :max-points 800
   :max-genome-size-in-initial-program 100
   :evalpush-limit 200
   :population-size 1000
   :max-generations 200
   :parent-selection :lexicase
   :epigenetic-markers [:close]
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 5
   :uniform-mutation-rate 0.01
   :problem-specific-report median-report
   :problem-specific-initial-report median-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1
   })



;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-evalpush-limit 200)

(reset! global-max-points 800)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
(def tom-program
  '(in1 in2 integer_min in3 in1 in2 integer_max integer_min integer_max print_integer))

#_(def tom-program-BAD
  '(integer_stackdepth integer_stackdepth exec_do*range
                       (exec_do*vector_integer in1) integer_dup 42 exec_do*times
                       (integer_mod integer_swap) integer_inc exec_do*times integer_lte
                       vector_integer_dup_times integer_stackdepth))

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

;(test-program-on-training tom-ind false)

(test-program-on-testing tom-ind false)

