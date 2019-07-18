;; bouncing_balls.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.bouncing-balls
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def bouncing-balls-atom-generators
  (concat (list
            (tag-instruction-erc [:exec :integer :boolean] 1000)
            (tagged-instruction-erc 1000)
            ;;; end ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :float])))

;; A list of data domains for the bouncing-balls problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def bouncing-balls-data-domains
  [[(list [0 0 0]
          [100 0.99 50]
          [3 0.66 1.5]
          [3 0.5 3]
          [1 0.3 10]) 5 0]
   [(fn [] (list (inc (rand 100)) (rand) (inc (rand 50)))) 195 2000]
   ])

;;Can make bouncing-balls test data like this:
;(test-and-train-data-from-domains bouncing-balls-data-domains)

; Helper function for error function
(defn bouncing-balls-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [[in1 in2 in3]]
          (vector [in1 in2 in3]
            (if (<= in1 in3) -1
              (loop [h in1 times 0]
                (cond
                  (<= h in3) (inc times)
                  :else (recur (* h in2) (inc times)))))))
       inputs))

(defn make-bouncing-balls-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-bouncing-balls-error-function
    ([individual]
      (the-actual-bouncing-balls-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-bouncing-balls-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input1 input2 input3] out-int] (case data-cases
                                                              :train train-cases
                                                              :test test-cases
                                                              [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input3 :input)
                                                     (push-item input2 :input)
                                                     (push-item input1 :input)))
                             printed-result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %-19s | Program output: %-19s" (str out-int) printed-result)))
                         ; Record the behavior
                         (swap! behavior conj printed-result)
                         ; Each test case is integer error
                         (if (number? printed-result)
                           (abs (- printed-result out-int)) ;distance from correct integer
                           1000000))))] ; penalty for no return value
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-bouncing-balls-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map bouncing-balls-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def bouncing-balls-train-and-test-cases
  (get-bouncing-balls-train-and-test bouncing-balls-data-domains))

(defn bouncing-balls-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first bouncing-balls-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second bouncing-balls-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn bouncing-balls-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Bouncing Balls problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-bouncing-balls-error-function-from-cases (first bouncing-balls-train-and-test-cases)
                                                          (second bouncing-balls-train-and-test-cases))
   :atom-generators bouncing-balls-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 1500
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :epigenetic-markers [:close]
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 5
   :uniform-mutation-rate 0.01
   :problem-specific-report bouncing-balls-report
   :problem-specific-initial-report bouncing-balls-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
