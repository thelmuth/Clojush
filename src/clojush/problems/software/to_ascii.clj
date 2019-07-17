;; to_ascii.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.to-ascii
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower])
  (:require [clojure.string :as string]))

; Atom generators
(def to-ascii-atom-generators
  (concat (list
            ;;; end constants
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec :char :string] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :string :char])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def to-ascii-data-domains
  [[(list 0
          1
          255
          10000
          121
          1010) 6 0] ;; Edge cases
   [(fn [] (lrand-int 10000)) 94 1000] ;; Random cases
   ])

;;Can make to-ascii test data like this:
;(test-and-train-data-from-domains to-ascii-data-domains)

; Helper function for error function
(defn to-ascii-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
            (char in)))
       inputs))

(defn make-to-ascii-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-to-ascii-error-function
    ([individual]
      (the-actual-to-ascii-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-to-ascii-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                      (doall
                         (for [[input1 correct-output] (case data-cases
                                                          :train train-cases
                                                          :test test-cases
                                                          [])]
                           (let [final-state (run-push (:program individual)
                                                       (->> (make-push-state)
                                                         (push-item input1 :input)))
                                 result (top-item :char final-state)]
                             (when print-outputs
                               (println (format "| Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                             ; Record the behavior
                             (swap! behavior conj result)
                             ; Error is
                             ; 1. Ascii Difference between the two characters
                               (if (char? result)
                                (abs (- (int correct-output) (int result)))
                                10000)
                             ))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-to-ascii-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map to-ascii-test-cases
      (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def to-ascii-train-and-test-cases
  (get-to-ascii-train-and-test to-ascii-data-domains))

(defn to-ascii-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first to-ascii-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second to-ascii-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn to-ascii-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- To Ascii problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-to-ascii-error-function-from-cases (first to-ascii-train-and-test-cases)
                                                                (second to-ascii-train-and-test-cases))
   :atom-generators to-ascii-atom-generators
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
   :problem-specific-report to-ascii-report
   :problem-specific-initial-report to-ascii-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
