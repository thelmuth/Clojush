;; bouncy_numbers.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.bouncy-numbers
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def bouncy-numbers-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def bouncy-numbers-data-domains
  [[(list 0
          1
          50000
          48000
          100
          500
          10000
          ) 7 0]
   [(fn [] (lrand-int 50000)) 193 2000]
   ])

;;Can make Bouncy Numbers test data like this:
; (map sort (test-and-train-data-from-domains bouncy-numbers-data-domains))

; Given a number, determine if it is bouncy
(defn bounce
  [num]
  (let [digits (map #(Character/digit % 10) (str num))]
  (if (or
        (apply <= digits)
        (apply >= digits)) false true)))

; All bouncy numbers below 50,000
(def bouncy-number-seq
  (filter bounce (range 50000)))

(defn bouncy-numbers-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
        (vector in
          (count (filter true? (map #(> in %) bouncy-number-seq)))))
       inputs))

(defn make-bouncy-numbers-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-bouncy-numbers-error-function
    ([individual]
      (the-actual-bouncy-numbers-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-bouncy-numbers-error-function individual data-cases false))
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
                               result (top-item :integer final-state)]
                             (when print-outputs
                               (println (format "Correct output: %s | Program output: %s" correct-output (str result))))
                           ; Record the behavior
                           (swap! behavior conj result)
                           ; Error is integer difference
                           (if (number? result)
                             (abs (- result correct-output)) ;distance from correct integer
                             1000000))))] ; penalty for no return value
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-bouncy-numbers-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map bouncy-numbers-test-cases
          (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def bouncy-numbers-train-and-test-cases
  (get-bouncy-numbers-train-and-test bouncy-numbers-data-domains))

(defn bouncy-numbers-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first bouncy-numbers-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second bouncy-numbers-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn bouncy-numbers-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Bouncy Numbers problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-bouncy-numbers-error-function-from-cases (first bouncy-numbers-train-and-test-cases)
                                                             (second bouncy-numbers-train-and-test-cases))
   :atom-generators bouncy-numbers-atom-generators
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
   :problem-specific-report bouncy-numbers-report
   :problem-specific-initial-report bouncy-numbers-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 0.001
   :max-error 1000000.0
   })
