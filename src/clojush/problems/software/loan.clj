;; loan.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.loan
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def loan-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :float :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :float :boolean :exec])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def loan-data-domains
  [[(list [1 0 1]   ; min size
          [50 50 50]   ; all same number
          [1000000 99.99 1000]   ; max size
          [5000 0 60]   ; no interest
          ) 4 0]
   [(fn [] (vector (inc (lrand 1000000)) (lrand 100) (inc (lrand-int 1000)))) 196 2000]
   ])

;;Can make Loan test data like this:
; (map sort (test-and-train-data-from-domains loan-data-domains))


(defn loan-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2 input3] output]."
  [inputs]
  (map (fn [[in1 in2 in3]]
        (vector [in1 in2 in3]
          (let [percent (/ in2 100)
                percent-over-months (/ percent 12)
                divisor (/ (float (dec (expt (inc percent-over-months) in3)))
                           (float (* percent-over-months (expt (inc percent-over-months) in3))))]
             (if (= in2 0)
               (round-to-n-decimal-places (float (/ in1 in3)) 2)
               (round-to-n-decimal-places (/ in1 divisor) 2)))))
       inputs))

(defn make-loan-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-loan-error-function
    ([individual]
      (the-actual-loan-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-loan-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                       (for [[[input1 input2 input3] correct-output] (case data-cases
                                                                               :train train-cases
                                                                               :test test-cases
                                                                               [])]
                         (let [final-state (run-push (:program individual)
                                                     (->> (make-push-state)
                                                       (push-item input3 :input)
                                                       (push-item input2 :input)
                                                       (push-item input1 :input)))
                               result (top-item :float final-state)]
                           (when print-outputs
                             (let [res-str (if (float? result)
                                             (format "%f" result)
                                             (str result))]
                               (println (format "Correct output: %f | Program output: %s" (float correct-output) res-str))))
                           ; Record the behavior
                           (swap! behavior conj result)
                           ; Error is float error rounded to 2 decimal places
                           (round-to-n-decimal-places
                            (if (number? result)
                              (abs (- result correct-output)) ; distance from correct integer
                              1000000.0) ; penalty for no return value
                            2)
                             )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-loan-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map loan-test-cases
          (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def loan-train-and-test-cases
  (get-loan-train-and-test loan-data-domains))

(defn loan-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first loan-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second loan-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn loan-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Loan problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 0.001)
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
  {:error-function (make-loan-error-function-from-cases (first loan-train-and-test-cases)
                                                             (second loan-train-and-test-cases))
   :atom-generators loan-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
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
   :uniform-mutation-constant-tweak-rate 0.9
   :problem-specific-report loan-report
   :problem-specific-initial-report loan-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 0.001
   :max-error 1000000.0
   })
