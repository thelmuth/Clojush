;; coin_sums.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.coin-sums
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def coin-sums-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :float :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :vector_integer])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def coin-sums-data-domains
  [[(list [[2 4 6 7] 3]   ; min case 1
          [[1] 1]   ; min case 2
          [[1 2 3 4 5 6 7 8 9 10] 50]   ; max case 1
          [[41 42 43 44 45 46 47 48 49 50] 50]    ; max case 2
          [[1 2 5 10 20 50 100 200] 35]
          [[1 2 3] 5]
          ) 6 0]
   [(fn [] (let [change (vec (repeatedly (inc (rand-int 10)) #(inc (rand-int 50))))]
            (vector change (+ (apply max change) (rand-int (- 50 (apply max change))))))) 194 2000]
   ])

;;Can make Coin Sums test data like this:
; (map sort (test-and-train-data-from-domains coin-sums-data-domains))

; Helper function for coin-sums-test-cases
(defn counter
  [change m n]
  (cond
    (= n 0) 1
    (< n 0) 0
    (and (<= m 0) (>= n 1)) 0
    :else (+ (counter change (dec m) n) (counter change m (- n (nth change (dec m)))))))

(defn coin-sums-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [[in1 in2]]
        (vector [in1 in2]
          (counter in1 (count in1) in2)))
       inputs))

(defn make-coin-sums-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-coin-sums-error-function
    ([individual]
      (the-actual-coin-sums-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-coin-sums-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                       (for [[[input1 input2] correct-output] (case data-cases
                                                                               :train train-cases
                                                                               :test test-cases
                                                                               [])]
                         (let [final-state (run-push (:program individual)
                                                     (->> (make-push-state)
                                                       (push-item input2 :input)
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

(defn get-coin-sums-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map coin-sums-test-cases
          (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def coin-sums-train-and-test-cases
  (get-coin-sums-train-and-test coin-sums-data-domains))

(defn coin-sums-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first coin-sums-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second coin-sums-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn coin-sums-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Coin Sums problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-coin-sums-error-function-from-cases (first coin-sums-train-and-test-cases)
                                                             (second coin-sums-train-and-test-cases))
   :atom-generators coin-sums-atom-generators
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
   :uniform-mutation-constant-tweak-rate 0.9
   :problem-specific-report coin-sums-report
   :problem-specific-initial-report coin-sums-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 0.001
   :max-error 1000000.0
   })
