;; list_average.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.list-average
  (:require [clojure.math.combinatorics :as combo])
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def list-average-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec :vector_integer :float] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :vector_integer :float])))

(defn list-average-input
  "Makes a List Average input of length len."
  [len]
  (vec (repeatedly len #(inc (rand-int 100)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def list-average-data-domains
  [[(list [1]
          [1 50 100]
          [50 50 50 50 50]
          [100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100]
          ) 4 0]
   [(fn [] (list-average-input (inc (rand-int 20)))) 196 2000]])

;;Can make List Average test data like this:
;(test-and-train-data-from-domains list-average-data-domains)

(defn list-average-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
           (float
             (/
               (reduce + (map * in (take (count in) (range 0 100))))
               (reduce + in)))))
       inputs))

(defn make-list-average-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-list-average-error-function
    ([individual]
      (the-actual-list-average-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-list-average-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input correct-output] (case data-cases
                                                     :train train-cases
                                                     :test test-cases
                                                     [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                        (push-item input :input)))
                             result (stack-ref :float 0 final-state)]
                         (when print-outputs
                           (println (format "Correct output: %.3f | Program output: %.3f" correct-output result)))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is float error rounded to 3 decimal places
                         (round-to-n-decimal-places
                          (if (number? result)
                            (abs (- result correct-output)) ; distance from correct integer
                            100000.0) ; penalty for no return value
                          3)
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-list-average-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map list-average-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def list-average-train-and-test-cases
  (get-list-average-train-and-test list-average-data-domains))

(defn list-average-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first list-average-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second list-average-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn list-average-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- List Average problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-list-average-error-function-from-cases (first list-average-train-and-test-cases)
                                                                   (second list-average-train-and-test-cases))
   :atom-generators list-average-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
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
   :problem-specific-report list-average-report
   :problem-specific-initial-report list-average-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 0.001
   :max-error 100000.0
   })
