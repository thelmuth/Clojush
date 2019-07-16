;; add_matrix.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.add-matrix
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

(defn make-row
  [len]
  (vec (repeatedly len #(rand-int 1000))))

;; Define test cases
(defn add-matrix-input
  "Makes an Add Matrix input vector of length row,col."
  [row col]
  (vector
    (vec (repeatedly col #(make-row row)))
    (vec (repeatedly col #(make-row row)))))

; Atom generators
(def add-matrix-atom-generators
  (concat (list
            []
            ;;; end constants
            (fn [] (add-matrix-input (inc (lrand-int 50)) (inc (lrand-int 50)))) ;Vector ERC
            ;;; end ERCs
            (tag-instruction-erc [:vector_integer :integer :boolean :exec :vector_vector_integer] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:vector_integer :integer :boolean :exec :vector_vector_integer])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def add-matrix-data-domains
  [[(list [[[]] [[]]]
          [[[1]] [[1]]]
          [[[1 2 3 4]] [[1 2 3 4]]]
          [[[1] [2] [3] [4]] [[1] [2] [3] [4]]]
          [[[5 5 5] [5 5 5] [5 5 5]] [[6 6 6] [6 6 6] [6 6 6]]]) 5 0]
   [(fn [] (add-matrix-input (inc (lrand-int 50)) (inc (lrand-int 50)))) 95 1000] ;; Random length, random ints
   ])

;;Can make Add Matrix test data like this:
;(test-and-train-data-from-domains add-matrix-data-domains)

; Helper function for error function
(defn add-matrix-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [[in1 in2]]
        (vector [in1 in2]
          (vec
            (map vec
              (partition (count (first in1))
                (map (fn [num1 num2]
                   (+ num1 num2))
                    (flatten in1)
                    (flatten in2)))))))
       inputs))

(defn make-add-matrix-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-add-matrix-error-function
    ([individual]
     (the-actual-add-matrix-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-add-matrix-error-function individual data-cases false))
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
                           result (top-item :vector_vector_integer final-state)]
                       (when print-outputs
                           (println (format "Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is integer error at each position in the vectors, with additional penalties for incorrect size vector
                       (if (and (vector? result)
                                (vector? (first result)))
                         (+' (apply +' (map (fn [cor res]
                                              (abs (- cor res)))
                                            (flatten correct-output)
                                            (flatten result)))
                             (*' 10000 (abs (- (count correct-output) (count result)))) ; penalty of 10000 times difference in sizes of vectors
                             (*' 1000 (abs (- (count (first correct-output)) (count (first result))))))
                         1000000000) ; penalty for no return value
                       )))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-add-matrix-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map add-matrix-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def add-matrix-train-and-test-cases
  (get-add-matrix-train-and-test add-matrix-data-domains))

(defn add-matrix-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first add-matrix-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second add-matrix-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn add-matrix-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Add Matrix problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-add-matrix-error-function-from-cases (first add-matrix-train-and-test-cases)
                                                                  (second add-matrix-train-and-test-cases))
   :atom-generators add-matrix-atom-generators
   :max-points 2000
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
   :problem-specific-report add-matrix-report
   :problem-specific-initial-report add-matrix-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
