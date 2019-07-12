;; phone_number.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.phone-number
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def phone-number-atom-generators
  (concat (list
            "(000) 000-0000"
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec])))

;; Define test cases
(defn phone-number-input
  "Makes a Phone Number input vector of length len."
  [len]
  (vec (repeatedly 10 #(rand-int 10))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def phone-number-data-domains
  [[(list [1 2 3 4 5 6 7 8 9 0]
          [0 9 8 7 6 5 4 3 2 1]
          [1 1 1 1 1 1 1 1 1 1]
          [9 0 9 0 9 0 9 0 9 0]
          ) 4 0]
   [(fn [] (phone-number-input (inc (lrand-int 100)))) 196 2000] ;; Random length
   ])

;;Can make Phone Number test data like this:
;(test-and-train-data-from-domains phone-number-data-domains)

; Helper function for error function
(defn phone-number-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (format "(%d%d%d) %d%d%d-%d%d%d%d" (first in) (second in) (nth in 2) (nth in 3) (nth in 4)
                                               (nth in 5) (nth in 6) (nth in 7) (nth in 8) (last in))))
       inputs))

(defn make-phone-number-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-phone-number-error-function
    ([individual]
     (the-actual-phone-number-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-phone-number-error-function individual data-cases false))
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
                           result (top-item :string final-state)]
                       (when print-outputs
                           (println (format "Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is levenshtein distance
                         (levenshtein-distance correct-output (str result))
                         )))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-phone-number-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map phone-number-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def phone-number-train-and-test-cases
  (get-phone-number-train-and-test phone-number-data-domains))

(defn phone-number-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first phone-number-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second phone-number-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn phone-number-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Phone Number problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-phone-number-error-function-from-cases (first phone-number-train-and-test-cases)
                                                                  (second phone-number-train-and-test-cases))
   :atom-generators phone-number-atom-generators
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
   :problem-specific-report phone-number-report
   :problem-specific-initial-report phone-number-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
