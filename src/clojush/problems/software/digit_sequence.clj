;; digit_sequence.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.digit-sequence
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

;; Define test cases
(defn digit-sequence-input
  "Makes a Digit Sequence input."
  []
  (vector (inc (rand-int 4)) (inc (rand-int 36))))

; Atom generators
(def digit-sequence-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def digit-sequence-data-domains
  [[(list [1 1]
          [4 36]) 2 0]
   [(fn [] (digit-sequence-input)) 48 500]
   ])

;;Can make Digit Sequence test data like this:
;(test-and-train-data-from-domains digit-sequence-data-domains)

(defn to-digits
  [number]
  (map #(Character/digit % 10) (str number)))

(defn proper?
  [num add-to]
  (if (= add-to (apply + (to-digits num)))
      true
      false))

; Helper function for error function
(defn digit-sequence-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2]]
    (vector [in1 in2]
      (let [max-val (reduce * (repeat in1 10))
            digit-sequence (filter #(= (count (str %)) in1) (range max-val))
            proper-seq (filter #(proper? % in2) digit-sequence)]
        (if (= (count proper-seq) 0)
          [0 0 0]
          (vector
            (count proper-seq)
            (apply min proper-seq)
            (apply max proper-seq))))))
       inputs))

(defn make-digit-sequence-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-digit-sequence-error-function
    ([individual]
     (the-actual-digit-sequence-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-digit-sequence-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors
              (flatten
                (doall
                   (for [[[input1 input2] [correct-output1 correct-output2 correct-output3]] (case data-cases
                                                                                                 :train train-cases
                                                                                                 :test test-cases
                                                                                                 [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input2 :input)
                                                      (push-item input1 :input)))
                           result1 (stack-ref :integer 0 final-state)
                           result2 (try (stack-ref :integer 1 final-state)
                                        (catch Exception e :no-stack-item))
                           result3 (try (stack-ref :integer 2 final-state)
                                        (catch Exception e :no-stack-item))]
                       (when print-outputs
                           (println (format "Correct output: %s %s %s\n| Program output: %s %s %s\n" (str correct-output1) (str correct-output2) (str correct-output3) (str result1) (str result2) (str result3))))
                       ; Record the behavior
                       (swap! behavior conj result1 result2 result3)
                       ; Error is integer error at each position in the vectors, with additional penalties for incorrect size vector
                       (vector
                         (if (number? result1)
                           (abs (- result1 correct-output1)) ; distance from correct integer
                           1000000) ; penalty for no return value
                         (if (number? result2)
                           (abs (- result2 correct-output2)) ; distance from correct integer
                           1000000) ; penalty for no return value
                         (if (number? result3)
                           (abs (- result3 correct-output3)) ; distance from correct integer
                           1000000) ; penalty for no return value
                       )))))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-digit-sequence-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map digit-sequence-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def digit-sequence-train-and-test-cases
  (get-digit-sequence-train-and-test digit-sequence-data-domains))

(defn digit-sequence-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first digit-sequence-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second digit-sequence-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn digit-sequence-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-with-test (error-function best :test)
        best-test-errors (:test-errors best-with-test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Digit Sequence problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-digit-sequence-error-function-from-cases (first digit-sequence-train-and-test-cases)
                                                                  (second digit-sequence-train-and-test-cases))
   :atom-generators digit-sequence-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 2000
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
   :problem-specific-report digit-sequence-report
   :problem-specific-initial-report digit-sequence-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
