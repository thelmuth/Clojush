;; greater_than_average.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.greater-than-average
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def greater-than-average-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:vector_float :float :integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:vector_float :float :boolean :integer :exec])))


;; Define test cases
(defn greater-than-average-input
  "Makes a Greater Than Average input vector of length len."
  [len]
  (vec (repeatedly len #(lrand 10000))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def greater-than-average-data-domains
  [[(list [0.0] [100.0] [1000.0] [10000.0] [9999.9999]) 5 0] ;; Length-1 vectors
   [(list [2.0 129.0]
          [0.12345 4.678]
          [999.99 74.113]
          [987.654321 995.0003]
          [788.788 812.19]) 5 0] ;; Length-2 vectors
   [(fn [] (greater-than-average-input 2)) 45 500] ;; Random Length-2 vectors
   [(fn [] (greater-than-average-input (+ 3 (lrand-int 3)))) 50 500] ;; Random Length-3, -4, and -5 vectors
   [(fn [] (greater-than-average-input 50)) 50 250] ;; Random Length-50 vectors
   [(fn [] (greater-than-average-input (inc (lrand-int 50)))) 95 1000] ;; Random length, random floats
   ])

;;Can make Greater Than Average test data like this:
;(test-and-train-data-from-domains greater-than-average-data-domains)

; Helper function for error function
(defn greater-than-average-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
            (vec (filter #(> % (/ (reduce + in) (count in))) in))))
       inputs))

(defn make-greater-than-average-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-greater-than-average-error-function
    ([individual]
     (the-actual-greater-than-average-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-greater-than-average-error-function individual data-cases false))
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
                           result (top-item :vector_float final-state)]
                       (when print-outputs
                          (println (format "Correct output: %s | Program output: %s" correct-output result)))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is integer error at each position in the vectors, with additional penalties for incorrect size vector
                       (if (vector? result)
                         (+' (apply +' (map (fn [cor res]
                                              (abs (- cor res)))
                                            correct-output
                                            result))
                             (*' 10000 (abs (- (count correct-output) (count result))))) ; penalty of 10000 times difference in sizes of vectors
                         1000000000) ; penalty for no return value
                       )))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-greater-than-average-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map greater-than-average-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def greater-than-average-train-and-test-cases
  (get-greater-than-average-train-and-test greater-than-average-data-domains))

(defn greater-than-average-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first greater-than-average-train-and-test-cases))]
    (println (format "Train Case: %3s | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second greater-than-average-train-and-test-cases))]
    (println (format "Test Case: %3s | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn greater-than-average-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Greater Than Average problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 1.0E-3)
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
  {:error-function (make-greater-than-average-error-function-from-cases (first greater-than-average-train-and-test-cases)
                                                                  (second greater-than-average-train-and-test-cases))
   :atom-generators greater-than-average-atom-generators
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
   :problem-specific-report greater-than-average-report
   :problem-specific-initial-report greater-than-average-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 1.0E-3
   :max-error 1000000.0
   })
