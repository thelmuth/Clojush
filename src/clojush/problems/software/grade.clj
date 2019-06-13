;; grade.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given 5 integer inputs, all in range [0,100]. The first four represent the
;; lower numeric thresholds for achieving an A, B, C, and D, and will be
;; distinct and in descending order. The fifth represents the student's numeric
;; grade. The program must output "Student has a X grade.", where X is A, B, C,
;; D, or F depending on the thresholds and their numeric grade.
;;
;; input stack has 5 integers

(ns clojush.problems.software.grade
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def grade-atom-generators
  (concat (list
            "Student has a "
            " grade."
            "A"
            "B"
            "C"
            "D"
            "F"
            ;;; end constants
            (fn [] (lrand-int 101)) ;Integer ERC [0,100]
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :string :boolean] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tagERCs
            'in1
            'in2
            'in3
            'in4
            'in5
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :exec :print])))

(defn make-grade-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-grade-error-function
    ([individual]
      (the-actual-grade-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-grade-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                     (doall
                       (for [[[input1 input2 input3 input4 input5] correct-output] (case data-cases
                                                                                     :train train-cases
                                                                                     :test test-cases
                                                                                     [])]
                         (let [final-state (run-push (:program individual)
                                                     (->> (make-push-state)
                                                       (push-item input5 :input)
                                                       (push-item input4 :input)
                                                       (push-item input3 :input)
                                                       (push-item input2 :input)
                                                       (push-item input1 :input)
                                                       (push-item "" :output)))
                               printed-result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "Correct output: %-19s | Program output: %-19s" (pr-str correct-output) (pr-str printed-result))))
                           ; Record the behavior
                           (swap! behavior conj printed-result)
                           ; Error is Levenshtein distance and, if correct format, distance from correct letter grade character
                           (vector
                             (levenshtein-distance correct-output printed-result)
                             (let [printed-letter (second (re-find #"^Student has a (.) grade.$" printed-result))
                                   correct-letter (second (re-find #"^Student has a (.) grade.$" correct-output))]
                               (if printed-letter
                                 (abs (- (int (first correct-letter))
                                         (int (first printed-letter)))) ;distance from correct character
                                 1000))
                             )))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def grade-train-and-test-cases
  (train-and-test-cases-from-dataset "grade" 159 2000))

(defn grade-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first grade-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second grade-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn grade-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Grade problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-grade-error-function-from-cases (first grade-train-and-test-cases)
                                                         (second grade-train-and-test-cases))
   :atom-generators grade-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 800
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
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report grade-report
   :problem-specific-initial-report grade-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
