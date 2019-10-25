;; vector_average.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of floats with length in [1,50], with each float in [-1000,1000],
;; return the average of those floats. Results are rounded to 4 decimal places.
;;
;; input stack has 1 input vector of floats

(ns clojush.problems.software.vector-average
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def vector-average-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:vector_float :float :integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:vector_float :float :integer :exec])))

(defn make-vector-average-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-vector-average-error-function
    ([individual]
     (the-actual-vector-average-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-vector-average-error-function individual data-cases false))
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
                         result (top-item :float final-state)]



                     
                                        ; print if wrong answer
                       (when (not= result correct-output)
                         (println "############################################################")
                         (println "Wrong result:" input1 "||" correct-output result)
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
(def vector-average-train-and-test-cases
  (train-and-test-cases-from-dataset "vector-average" 0 25500000))

(defn vector-average-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first vector-average-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second vector-average-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn vector-average-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Vector Average problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 1.0E-3)
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
  {:error-function (make-vector-average-error-function-from-cases (first vector-average-train-and-test-cases)
                                                                  (second vector-average-train-and-test-cases))
   :atom-generators vector-average-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 800
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
   :problem-specific-report vector-average-report
   :problem-specific-initial-report vector-average-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 1.0E-3
   :max-error 1000000.0
   })


;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-max-points 3200)

(reset! global-evalpush-limit 1600)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
(def tom-program
  '(in1 integer_stackdepth exec_do*vector_float (float_add integer_stackdepth) float_stackdepth float_dup float_dup_items integer_div float_mod integer_dec exec_do*range float_inc float_div)
  )


;; This program is hand-written
#_(def tom-program
 '(
    in1 integer_inc exec_do*count
    (
      integer_dup integer_mult integer_add
      )
    ))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind false)


;(test-program-on-testing tom-ind false)


