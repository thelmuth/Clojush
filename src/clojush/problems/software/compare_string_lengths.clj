;; compare_string_lengths.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given three strings in1, in2, and in3, return true if
;; lenth(in1) < length(in2) < length(in3), and false otherwise.
;;
;; input stack has 3 input strings

(ns clojush.problems.software.compare-string-lengths
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def csl-atom-generators
  (concat (list
            (fn [] (lrand-nth (list true false))) ;Boolean ERC
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :string :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :exec])))

(defn make-compare-string-lengths-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-csl-error-function
    ([individual]
      (the-actual-csl-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-csl-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input1 input2 input3 correct-output] (case data-cases
                                                                     :train train-cases
                                                                     :test test-cases
                                                                     [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input3 :input)
                                                     (push-item input2 :input)
                                                     (push-item input1 :input)))
                             result (top-item :boolean final-state)]
                         (when print-outputs
                           (println (format "Correct output: %5b | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is boolean error
                         (if (= result correct-output)
                           0
                           1))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def compare-string-lengths-train-and-test-cases
  (train-and-test-cases-from-dataset "compare-string-lengths" 78 1000))

(defn compare-string-lengths-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first compare-string-lengths-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second compare-string-lengths-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn csl-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Compare String Lengths problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-compare-string-lengths-error-function-from-cases (first compare-string-lengths-train-and-test-cases)
                                                                          (second compare-string-lengths-train-and-test-cases))
   :atom-generators csl-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
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
   :problem-specific-report csl-report
   :problem-specific-initial-report compare-string-lengths-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1
   })
