;; rgb.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.rgb
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower])
  (:require [clojure.string :as string]))

; Atom generators
(def rgb-atom-generators
  (concat (list
            \A
            \B
            \C
            \D
            \E
            \F
            0
            255
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :print :vector_integer :string :char])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def rgb-data-domains
  [[(list [0 0 0]
          [255 255 255]
          [255 0 0]
          [0 255 0]
          [0 0 255]
          [100 100 100]) 6 0] ;; Edge cases
   [(fn [] (vector (rand-int 256) (rand-int 256) (rand-int 256))) 194 2000] ;; Random cases
   ])

;;Can make RGB test data like this:
;(test-and-train-data-from-domains rgb-data-domains)

; Helper function for error function
(defn rgb-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
            (format "%02X%02X%02X" (first in) (second in) (last in))))
       inputs))

(defn make-rgb-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-rgb-error-function
    ([individual]
      (the-actual-rgb-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-rgb-error-function individual data-cases false))
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
                                                         (push-item input1 :input)
                                                         (push-item "" :output)))
                                 result (stack-ref :output 0 final-state)]
                             (when print-outputs
                               (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                             ; Record the behavior
                             (swap! behavior conj result)
                             ; Error is
                             ; 1. Levenshtein distance of printed strings
                             ; 2. The number of characters in the string that aren't hex digits (0-9, a-f)
                             (+
                               (levenshtein-distance correct-output (str result))
                               (reduce + (map #(if (clojure.string/includes? "0123456789ABCDEF" (str (first %))) 0 (second %)) (frequencies (str result))))
                           )))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-rgb-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map rgb-test-cases
      (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def rgb-train-and-test-cases
  (get-rgb-train-and-test rgb-data-domains))

(defn rgb-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first rgb-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second rgb-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn rgb-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- RGB problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-rgb-error-function-from-cases (first rgb-train-and-test-cases)
                                                                (second rgb-train-and-test-cases))
   :atom-generators rgb-atom-generators
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
   :problem-specific-report rgb-report
   :problem-specific-initial-report rgb-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
