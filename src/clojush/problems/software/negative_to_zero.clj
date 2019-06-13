;; negative_to_zero.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of integers in [-1000,1000] with length <= 50, return the
;; vector where all negative integers have been replaced by 0.
;;
;; NOTE: This problem gets lots of solutions that don't generalize. We could add
;; another error that finds the integer error at each position in vector with
;; penalty for wrong size of vector, which might help with generalization (but might not).
;;
;; input stack has 1 input vector of integers

(ns clojush.problems.software.negative-to-zero
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def negative-to-zero-atom-generators
  (concat (list
            0
            []
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :vector_integer :exec])))

(defn make-negative-to-zero-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-negative-to-zero-error-function
    ([individual]
      (the-actual-negative-to-zero-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-negative-to-zero-error-function individual data-cases false))
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
                             result (top-item :vector_integer final-state)]
                         (when print-outputs
                           (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is Levenshtein distance of vectors
                         (if (vector? result)
                           (levenshtein-distance correct-output result)
                           5000) ; penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def negative-to-zero-train-and-test-cases
  (train-and-test-cases-from-dataset "negative-to-zero" 183 2000))

(defn negative-to-zero-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first negative-to-zero-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second negative-to-zero-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn negative-to-zero-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Negative To Zero problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-negative-to-zero-error-function-from-cases (first negative-to-zero-train-and-test-cases)
                                                            (second negative-to-zero-train-and-test-cases))
   :atom-generators negative-to-zero-atom-generators
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
   :problem-specific-report negative-to-zero-report
   :problem-specific-initial-report negative-to-zero-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
