;; small_or_large.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given an integer n in the range [-10000, 10000], print "small" if n < 1000
;; and "large" if n >= 2000 (and nothing if 1000 <= n < 2000).
;;
;; input stack has input integer n

(ns clojush.problems.software.small-or-large
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def small-or-large-atom-generators
  (concat (list
            "small"
            "large"
            ;;; end constants
            (fn [] (- (lrand-int 20001) 10000)) ;Integer ERC [-10000,10000]
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :string :print])))

(defn make-small-or-large-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-small-or-large-error-function
    ([individual]
      (the-actual-small-or-large-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-small-or-large-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input1 correct-output] (case data-cases
                                                     :train train-cases
                                                     :test test-cases
                                                     [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input1 :input)
                                                     (push-item "" :output)))
                             result (stack-ref :output 0 final-state)]
                         (when print-outputs
                           (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is Levenshtein distance of printed strings
                         (levenshtein-distance correct-output result))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def small-or-large-train-and-test-cases
  (train-and-test-cases-from-dataset "small-or-large" 73 920))

(defn small-or-large-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first small-or-large-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second small-or-large-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn small-or-large-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Small Or Large problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-small-or-large-error-function-from-cases (first small-or-large-train-and-test-cases)
                                                                  (second small-or-large-train-and-test-cases))
   :atom-generators small-or-large-atom-generators
   :max-points 800
   :max-genome-size-in-initial-program 100
   :evalpush-limit 300
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 5
   :uniform-mutation-rate 0.01
   :problem-specific-report small-or-large-report
   :problem-specific-initial-report small-or-large-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
