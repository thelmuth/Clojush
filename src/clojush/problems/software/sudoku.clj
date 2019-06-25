;; sudoku.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.sudoku
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def sudoku-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :exec :boolean :vector_integer] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :exec :boolean :vector_vector_integer :vector_integer])))

(defn make-row
  []
  (vec (repeatedly 9 #(inc (rand-int 9)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def sudoku-data-domains
  [[(list []  ; empty
          [[1 2 3 4 5 6]
           [2 3 4 5 6 7]] ; too small
          [[2 9 5 7 4 3 8 6 1]
           [4 3 1 8 6 5 9 2 7]
           [8 7 6 1 9 2 5 4 3]
           [3 8 7 4 5 9 2 1 6]
           [6 1 2 3 8 7 4 9 5]
           [5 4 9 2 1 6 7 3 8]
           [7 6 3 5 2 4 1 8 9]
           [9 2 8 6 7 1 3 5 4]
           [1 5 4 9 3 8 6 7 2]] ; correct
          [[1 1 1 1 1 1 1 1 1]
           [1 1 1 1 1 1 1 1 1]
           [1 1 1 1 1 1 1 1 1]
           [1 1 1 1 1 1 1 1 1]
           [1 1 1 1 1 1 1 1 1]
           [1 1 1 1 1 1 1 1 1]
           [1 1 1 1 1 1 1 1 1]
           [1 1 1 1 1 1 1 1 1]
           [1 1 1 1 1 1 1 1 1]] ; all same number
          [[2 9 5 7 4 3 8 6 1]
           [4 3 1 8 6 5 9 2 7]
           [8 7 6 1 9 2 5 4 3]
           [3 8 7 4 5 9 2 1 6]
           [6 1 2 3 8 7 4 9 5]
           [5 4 9 2 1 1 7 3 8]
           [7 6 3 5 2 4 1 8 9]
           [9 2 8 6 7 1 3 5 4]
           [1 5 4 9 3 8 6 7 2]] ; almost correct
          [[1 5 2 4 8 9 3 7 6] ; correct
           [7 3 9 2 5 6 8 4 1]
           [4 6 8 3 7 1 2 9 5]
           [3 8 7 1 2 4 6 5 9]
           [5 9 1 7 6 3 4 2 8]
           [2 4 6 8 9 5 7 1 3]
           [9 1 4 6 3 7 5 8 2]
           [6 2 5 9 4 8 1 3 7]
           [8 7 3 5 1 2 9 6 4]]) 6 0]
   [(fn [] (vec (repeatedly 9 #(make-row)))) 194 2000]
  ])

;;Can make sudoku test data like this:
;(test-and-train-data-from-domains sudoku-data-domains)

; Helper function for the helper function below
(defn check
  [block]
  (clojure.set/subset? #{1 2 3 4 5 6 7 8 9} (set block)))

; Helper function for error function
(defn sudoku-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
           (if (not= (count in) 81)
               false
               (if (and (every? true?
                              (map check (partition 9 (flatten (vector
                                         (partition 9 (vec (apply concat (apply map vector (take 3 (partition 9 in))))))
                                         (partition 9 (vec (apply concat (apply map vector (partition 9 (subvec in 27 54))))))
                                         (partition 9 (vec (apply concat (apply map vector (nthrest (partition 9 in) 6)))))))))) ; squares
                        (every? true? (map check (apply map vector (partition 9 in)))) ; columns
                        (every? true? (map check (partition 9 in)))) true false))))
       inputs))

(defn make-sudoku-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-sudoku-error-function
    ([individual]
      (the-actual-sudoku-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-sudoku-error-function individual data-cases false))
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
                             result (top-item :boolean final-state)]
                         (when print-outputs
                           (println (format "Correct output: %5b | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is integer distance
                         ; Error is boolean error
                         (if (= result correct-output)
                           0
                           1))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-sudoku-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map sudoku-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def sudoku-train-and-test-cases
  (get-sudoku-train-and-test sudoku-data-domains))

(defn sudoku-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first sudoku-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second sudoku-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn sudoku-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- sudoku problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-sudoku-error-function-from-cases (first sudoku-train-and-test-cases)
                                                                  (second sudoku-train-and-test-cases))
   :atom-generators sudoku-atom-generators
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
   :problem-specific-report sudoku-report
   :problem-specific-initial-report sudoku-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
