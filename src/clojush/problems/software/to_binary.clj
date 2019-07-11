;; to_binary.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.to-binary
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower])
  (:require [clojure.string :as string]))

; Atom generators
(def to-binary-atom-generators
  (concat (list
            \0
            \1
            ;;; end constants
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
(def to-binary-data-domains
  [[(list 0
          1
          0
          255
          500000
          65535
          123456
          1010) 8 0] ;; Edge cases
   [(fn [] (lrand-int 500000)) 192 2000] ;; Random cases
   ])

;;Can make to-binary test data like this:
;(test-and-train-data-from-domains to-binary-data-domains)

; Helper function for error function
(defn to-binary-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
            (Integer/toBinaryString in)))
       inputs))

(defn make-to-binary-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-to-binary-error-function
    ([individual]
      (the-actual-to-binary-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-to-binary-error-function individual data-cases false))
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
                                 result (top-item :string final-state)]
                             (when print-outputs
                               (println (format "| Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                             ; Record the behavior
                             (swap! behavior conj result)
                             ; Error is
                             ; 1. Levenshtein distance of printed strings
                             ; 2. The number of characters in the string that aren't hex digits (0-9, a-f)
                             (+
                               (levenshtein-distance correct-output (str result))
                               (reduce + (map #(if (clojure.string/includes? "01" (str (first %))) 0 (second %)) (frequencies (str result))))
                             )))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-to-binary-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map to-binary-test-cases
      (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def to-binary-train-and-test-cases
  (get-to-binary-train-and-test to-binary-data-domains))

(defn to-binary-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first to-binary-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second to-binary-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn to-binary-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- To Binary problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-to-binary-error-function-from-cases (first to-binary-train-and-test-cases)
                                                                (second to-binary-train-and-test-cases))
   :atom-generators to-binary-atom-generators
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
   :problem-specific-report to-binary-report
   :problem-specific-initial-report to-binary-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
