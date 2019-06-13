;; super_anagrams.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given strings x and y of lowercase letters with length <= 20, return true if
;; y is a super anagram of x, which is the case if every character in x is in y.
;; To be true, y may contain extra characters, but must have at least as many
;; copies of each character as x does.
;;
;; input stack has the 2 input strings
;;

(ns clojush.problems.software.super-anagrams
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

; Atom generators
(def super-anagrams-atom-generators
  (concat (list
            ;;; end constants
            (fn [] (lrand-nth (list true false))) ;Boolean ERC
            (fn [] (- (lrand-int 2001) 1000)) ;Integer ERC [-1000,1000]
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec])))

(defn make-super-anagrams-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-super-anagrams-error-function
    ([individual]
      (the-actual-super-anagrams-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-super-anagrams-error-function individual data-cases false))
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
                                                     (push-item input1 :input)
                                                     (push-item "" :output)))
                             result (top-item :boolean final-state)]
                         (when print-outputs
                           (println (format "Correct output: %5b | Program output: %s" correct-output (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is boolean error
                         (if (= result correct-output)
                           0
                           1))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def super-anagrams-train-and-test-cases
  (map #(sort-by second %)
    (train-and-test-cases-from-dataset "super-anagrams" 170 2000)))

(defn super-anagrams-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first super-anagrams-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second super-anagrams-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn super-anagrams-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Super Anagrams problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-super-anagrams-error-function-from-cases (first super-anagrams-train-and-test-cases)
                                                                  (second super-anagrams-train-and-test-cases))
   :atom-generators super-anagrams-atom-generators
   :max-points 3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit 1600
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
   :problem-specific-report super-anagrams-report
   :problem-specific-initial-report super-anagrams-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1
   })
