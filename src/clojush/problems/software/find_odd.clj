;; find_odd.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.find-odd
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def find-odd-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :vector_integer :exec])))


;; Define test cases
(defn find-odd-input
  "Makes a Find Odd input vector of terms terms."
  [terms]
  (let [nums (flatten (repeatedly terms #(repeat (rand-nth '(2 4 6 8 10)) (rand-int 10000))))]
    (shuffle (conj nums (rand-nth nums)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def find-odd-data-domains
  [[(list [1]
          [1 1 1 1 2 2 2]
          [2 2 4 4 3 3 3 5 5]) 3 0]
   [(fn [] (find-odd-input (inc (lrand-int 30)))) 97 1000] ;; Random length, random prob of odd
   ])

;;Can make Find Odd test data like this:
;(test-and-train-data-from-domains find-odd-data-domains)

; Helper function for error function
(defn find-odd-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
    (vector in
      (first
        (nth (map vec (frequencies in))
          (.indexOf
            (map #(odd? (second %)) (map vec (frequencies in)))
            true)))))
       inputs))

(defn make-find-odd-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-find-odd-error-function
    ([individual]
      (the-actual-find-odd-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-find-odd-error-function individual data-cases false))
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
                           (println (format "Correct output: %2d | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is integer error
                         (if (number? result)
                           (abs (- result correct-output)) ; distance from correct integer
                           100000) ; penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-find-odd-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map find-odd-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def find-odd-train-and-test-cases
  (get-find-odd-train-and-test find-odd-data-domains))

(defn find-odd-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first find-odd-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second find-odd-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn find-odd-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Find Odd problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-find-odd-error-function-from-cases (first find-odd-train-and-test-cases)
                                                              (second find-odd-train-and-test-cases))
   :atom-generators find-odd-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
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
   :problem-specific-report find-odd-report
   :problem-specific-initial-report find-odd-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000
   })
