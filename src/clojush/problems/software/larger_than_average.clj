;; larger_than_average.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.larger-than-average
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

;; Define test cases
(defn larger-than-average-input
  "Makes a Larger than Average input vector of length len."
  [len]
  (vec (repeatedly len #(- (* (lrand) 2000.0) 1000.0))))

(defn skewed-larger-than-average-input
  "Makes an input vector for Larger than Average that is skewed."
  [len]
  (let [groupings (loop [groups '()
                         num-left len]
                    (if (<= num-left 0)
                      groups
                      (let [next-num (inc (rand-int num-left))]
                        (recur (conj groups next-num)
                               (- num-left next-num)))))]
    (flatten (map (fn [size]
                    (let [center (- (rand 2000.0) 1000.0)]
                      (repeatedly size #(+ center -5 (rand 10))))) ; Add uniform noise [-5, 5] to center
                  groupings))))

; Atom generators
(def larger-than-average-atom-generators
  (concat (list
            ;;; end constants
            (fn [] (larger-than-average-input (inc (lrand-int 50)))) ;Vector ERC
            ;;; end ERCs
            (tag-instruction-erc [:vector_float :float :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:vector_float :float :integer :boolean :exec])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def larger-than-average-data-domains
  [[(list [0.0] [100.23405] [500.12942] [1000.00000] [1.42434]) 5 0] ;; Length-1 vectors
   [(list [1.47082 1.98723]
          [1.00001 999.24423]
          [432.19342 14.35351]
          [987.74234 910.31289]
          [0.12337 999.72791]) 5 0] ;; Length-2 vectors
   [(fn [] (larger-than-average-input 3)) 15 50] ;; Random Length-3 vectors
   [(fn [] (larger-than-average-input 4)) 15 50] ;; Random Length-4 vectors
   [(fn [] (larger-than-average-input 5)) 15 50] ;; Random Length-5 vectors
   [(list [3.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0]
          [-56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 -56.11 72.93]
          [-100.12 -104.23 -102.49 -99.32 938.12 -98.43 -100.65 -100.24 -103.65 832.94 768.73 -105.34 -100.98 -103.68 982.42 -102.46 -99.48 987.13 -106.42 -93.19]
          [1000.00000 999.99999 0.0 -999.99999 -10000.00000]
          [1000.0 -385.3453 -491.42379 -362.901 -943.3482 -423.789]) 5 0] ; "Weird" cases
   [(fn [] (larger-than-average-input 20)) 20 250] ;; Random Length-20 vectors
   [(fn [] (skewed-larger-than-average-input (inc (lrand-int 20)))) 60 750]
   [(fn [] (larger-than-average-input (inc (lrand-int 20)))) 60 850] ;; Random length, random ints
   ])
   ; -45 train cases, -650 test cases

;;Can make Larger than Average test data like this:
;(test-and-train-data-from-domains larger-than-average-data-domains)

; Helper function for error function
(defn larger-than-average-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
    (vector in
      (let [avg (/ (reduce + in) (count in))]
        (count (filter #(> % avg) in)))))
       inputs))

(defn make-larger-than-average-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-larger-than-average-error-function
    ([individual]
     (the-actual-larger-than-average-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-larger-than-average-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors
                (doall
                   (for [[input1 correct-output] (case data-cases
                                                   :train train-cases
                                                   :test test-cases
                                                   [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)))
                           result (stack-ref :integer 0 final-state)]
                       (when print-outputs
                           (println (format "Correct output: %s\n| Program output: %s" (str correct-output) (str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is integer error
                       (if (number? result)
                           (abs (- result correct-output)) ; distance from correct integer
                           10000) ; penalty for no return value
                       )))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-larger-than-average-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map larger-than-average-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def larger-than-average-train-and-test-cases
  (get-larger-than-average-train-and-test larger-than-average-data-domains))

(defn larger-than-average-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first larger-than-average-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second larger-than-average-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn larger-than-average-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-with-test (error-function best :test)
        best-test-errors (:test-errors best-with-test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Larger than Average problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-larger-than-average-error-function-from-cases (first larger-than-average-train-and-test-cases)
                                                                  (second larger-than-average-train-and-test-cases))
   :atom-generators larger-than-average-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 4000
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
   :problem-specific-report larger-than-average-report
   :problem-specific-initial-report larger-than-average-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 10000
   })
