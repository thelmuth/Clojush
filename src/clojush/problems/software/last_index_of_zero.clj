;; last_index_of_zero.clj
;; Nic McPhee, mcphee@morris.umn.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers of length <= 50, each integer in the range [-50,50],
;; at least one of which is 0, return the index of the last occurance of 0 in the vector.
;;
;; input stack has 1 input vector of integers

(ns clojush.problems.software.last-index-of-zero
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def last-index-of-zero-atom-generators
  (concat (list
            ^{:generator-label "Random numbers in the range [-50,50]"}
            (fn [] (- (lrand-int 101) 50))
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :vector_integer :exec])))

(defn make-last-index-of-zero-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-last-index-of-zero-error-function
    ([individual]
      (the-actual-last-index-of-zero-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-last-index-of-zero-error-function individual data-cases false))
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
                             result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %2d | Program output: %s"
                                            correct-output
                                            (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is absolute distance from correct index
                         (if (number? result)
                           (abs (- result correct-output)) ; distance from correct integer
                           1000000) ; penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def last-index-of-zero-train-and-test-cases
  (train-and-test-cases-from-dataset "last-index-of-zero" 78 1000))

(defn last-index-of-zero-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first last-index-of-zero-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second last-index-of-zero-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn last-index-of-zero-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Last Index of Zero problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-last-index-of-zero-error-function-from-cases (first last-index-of-zero-train-and-test-cases)
                                                                      (second last-index-of-zero-train-and-test-cases))
   :atom-generators last-index-of-zero-atom-generators
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
   :problem-specific-report last-index-of-zero-report
   :problem-specific-initial-report last-index-of-zero-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
