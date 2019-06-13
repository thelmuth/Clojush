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
           errors (doall
                   (for [[input1 correct-output] (case data-cases
                                                   :train train-cases
                                                   :test test-cases
                                                   [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)))
                           result (top-item :float final-state)]
                       (when print-outputs
                         (let [res-str (if (float? result)
                                         (format "%19.14f" result)
                                         (str result))]
                           (println (format "Correct output: %19.14f | Program output: %s" correct-output res-str))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is float error rounded to 4 decimal places
                       (round-to-n-decimal-places
                        (if (number? result)
                          (abs (- result correct-output)) ; distance from correct integer
                          1000000.0) ; penalty for no return value
                        4)
                       )))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

; Define train and test cases
(def vector-average-train-and-test-cases
  (train-and-test-cases-from-dataset "vector-average" 240 2550))

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
