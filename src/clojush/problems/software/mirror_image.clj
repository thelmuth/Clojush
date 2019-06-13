;; mirror_image.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given two lists of integers of the same length <= 50, return true if one
;; list is the reverse of the other, and false otherwise.
;;
;; input stack has 2 input vectors of integers

(ns clojush.problems.software.mirror-image
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def mirror-image-atom-generators
  (concat (list
            (fn [] (lrand-nth (list true false))) ;Boolean
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :vector_integer :exec])))

; Define train and test cases
(def mirror-image-train-and-test-cases
  (train-and-test-cases-from-dataset "mirror-image" 77 1000))

(defn mirror-image-evaluate-program-for-behaviors
  "Evaluates the program on the given list of cases.
   Returns the behaviors, a list of the outputs of the program on the inputs."
  [program cases]
  (doall
   (for [[[input1 input2] output] cases]
     (let [final-state (run-push program
                                 (->> (make-push-state)
                                      (push-item input2 :input)
                                      (push-item input1 :input)))]
       (top-item :boolean final-state)))))

(defn mirror-image-errors-from-behaviors
  "Takes a list of behaviors across the list of cases and finds the error
   for each of those behaviors, returning an error vector."
  [behaviors cases]
  (map (fn [result correct-output]
         (if (= result correct-output)
           0
           1))
       behaviors
       (map second cases)))

(defn mirror-image-error-function
  "The error function. Takes an individual as input,
   and returns that individual with :errors and :behaviors set."
  ([individual]
   (mirror-image-error-function individual :train))
  ([individual data-cases] ;; data-cases should be :train or :test
   (let [cases (case data-cases
                 :train (first mirror-image-train-and-test-cases)
                 :test (second mirror-image-train-and-test-cases)
                 [])
         behaviors (mirror-image-evaluate-program-for-behaviors (:program individual)
                                                                 cases)
         errors (mirror-image-errors-from-behaviors behaviors cases)]
     (cond
       (= data-cases :train) (assoc individual :behaviors behaviors :errors errors)
       (= data-cases :test) (assoc individual :test-errors errors)))))

(defn mirror-image-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first mirror-image-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second mirror-image-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn mirror-image-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-with-test (error-function best :test)
        best-test-errors (:test-errors best-with-test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Mirror Image problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (doseq [[correct-output result] (map vector
                                         (map second (first mirror-image-train-and-test-cases))
                                         (:behaviors best))]
      (println (format "Correct output: %5b | Program output: %s" correct-output (str result))))
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function mirror-image-error-function
   :atom-generators mirror-image-atom-generators
   :max-points 1200
   :max-genome-size-in-initial-program 150
   :evalpush-limit 600
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
   :problem-specific-report mirror-image-report
   :problem-specific-initial-report mirror-image-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1
   })
