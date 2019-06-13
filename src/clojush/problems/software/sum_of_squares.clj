;; sum_of_squares.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given integer 0 < n <= 100, return the sum of squaring each positive integer
;; between 1 and n inclusive.
;;
;; input stack has integer n

(ns clojush.problems.software.sum-of-squares
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def sum-of-squares-atom-generators
  (concat (list
            0
            1
            ;;; end constants
            (fn [] (- (lrand-int 201) 100)) ;Integer ERC [-100,100]
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec])))

(defn make-sum-of-squares-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-sum-of-squares-error-function
    ([individual]
      (the-actual-sum-of-squares-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-sum-of-squares-error-function individual data-cases false))
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
                             result (stack-ref :integer 0 final-state)]
                         (when print-outputs
                           (println (format "Correct output: %6d | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is integer distance
                         (if (number? result)
                           (abs (- result correct-output)) ;distance from correct integer
                           1000000000) ;penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def sum-of-squares-train-and-test-cases
  (train-and-test-cases-from-dataset "sum-of-squares" 44 100))

(defn sum-of-squares-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first sum-of-squares-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second sum-of-squares-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn sum-of-squares-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Sum Of Squares problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-sum-of-squares-error-function-from-cases (first sum-of-squares-train-and-test-cases)
                                                                  (second sum-of-squares-train-and-test-cases))
   :atom-generators sum-of-squares-atom-generators
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
   :problem-specific-report sum-of-squares-report
   :problem-specific-initial-report sum-of-squares-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
