;;sum_positive.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.sum-positive
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def sum-positive-atom-generators
  (concat (list
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:vector_integer :integer :boolean :exec])))

;; Define test cases
(defn sum-positive-input
  "Makes a Sum Positive input of length len"
  [len]
  (vec (repeatedly len #(- (lrand-int 2000) 1000))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def sum-positive-data-domains
  [[(list []
          [0]
          [1 2 3 4 5]
          [-1 -2 -3 -4 -5]
          [1000 1000 1000 1000 -1000]
          [1 -2 3 -4 5]
          [10]
          ) 7 0]
   [(fn [] (sum-positive-input (lrand-int 1000))) 193 2000]
   ])

;;Can make Sum Positive test data like this:
;(test-and-train-data-from-domains sum-positive-data-domains)

; Helper function for error function
(defn sum-positive-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (reduce + (filter pos-int? in))))
       inputs))

(defn make-sum-positive-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-sum-positive-error-function
    ([individual]
     (the-actual-sum-positive-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-sum-positive-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (doall
                   (for [[input correct-output] (case data-cases
                                                   :train train-cases
                                                   :test test-cases
                                                   [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input :input)))
                           result (top-item :integer final-state)]
                       (when print-outputs
                           (println (format "Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is integer difference
                       (if (number? result)
                         (abs (- result correct-output)) ;distance from correct integer
                         1000000))))] ; penalty for no return value
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-sum-positive-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sum-positive-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def sum-positive-train-and-test-cases
  (get-sum-positive-train-and-test sum-positive-data-domains))

(defn sum-positive-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first sum-positive-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second sum-positive-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn sum-positive-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Sum Positive problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-sum-positive-error-function-from-cases (first sum-positive-train-and-test-cases)
                                                                  (second sum-positive-train-and-test-cases))
   :atom-generators sum-positive-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 1200
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
   :problem-specific-report sum-positive-report
   :problem-specific-initial-report sum-positive-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
