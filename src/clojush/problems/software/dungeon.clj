;; dungeon.clj
;;
;; Peter Kelly (pxkelly@hamilton.edu)
;;
;; Problem Source: https://leetcode.com/problems/dungeon-game/
;;
;;

(ns clojush.problems.software.dungeon
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def dungeon-atom-generators
  (concat (list
            ;;; end ERCs
            (tag-instruction-erc [:integer :vector_integer :exec :boolean] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            'in3
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :vector_integer :exec :boolean])))

;; Define test cases
(defn dungeon-input
  "Makes a Dungeon input given a row and col"
  [row col]
  (vec (repeatedly (* row col) #(- (lrand-int 201) 100))))

(def dungeon-data-domains
  [[(list [3 3 [0 0 0 0 0 0 0 0 0]]
          [1 1 [-5]]
          [3 3 [-23 -54 -34 -65 -78 -96 -32 -21 -43]]
          [3 3 [43 45 12 76 87 2 48 98 1]]
          [2 2 [-100 -100 100 100]]
          [2 2 [100 -50 -50 -49]]) 6 0]
   [(fn [] (let [row (inc (lrand-int 20)) col (inc (lrand-int 20))]
                (vector row col (dungeon-input row col)))) 194 2000]
  ])

;;Can make dungeon test data like this:
;(test-and-train-data-from-domains dungeon-data-domains)

; Helper function for error function
(defn dungeon-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2 input3] output]."
  [inputs]
  (map (fn [row col dungeon]
          (loop [start-health 1 health-remain 1 current-spot 0]
            (cond
              (<= (+ health-remain (nth dungeon current-spot -1000)) 0) (recur (inc start-health) (inc start-health) 0) ; if the current spot kills you, then restart
              (= current-spot (dec (* row col))) (if (<= (+ health-remain (nth dungeon current-spot -1000)) 0) ; if the knight made it to the end, check if the final spot kills him
                                                (recur (inc start-health)
                                                       (inc start-health)
                                                       0)
                                                start-health) ; if it does, restart, otherwise, return the health
              (and (> (+ (+ health-remain (nth dungeon current-spot -1000)) (nth dungeon (+ current-spot 1) -1000)) 0)  ; if the knight could survive both directions
                   (> (+ (+ health-remain (nth dungeon current-spot -1000)) (nth dungeon (+ current-spot col) -1000)) 0)) (if (> (+ (+ health-remain (nth dungeon current-spot -1000)) (nth dungeon (+ current-spot 1) -1000))
                                                                                                                                 (+ (+ health-remain (nth dungeon current-spot -1000)) (nth dungeon (+ current-spot col) -1000)))
                                                                                                                          (recur   ; If going right results in having more health, go right
                                                                                                                            start-health
                                                                                                                            (+ health-remain (nth dungeon current-spot -1000))
                                                                                                                            (+ current-spot 1))
                                                                                                                          (recur  ; otherwise, go down
                                                                                                                            start-health
                                                                                                                            (+ health-remain (nth dungeon current-spot -1000))
                                                                                                                            (+ current-spot col)))
              (> (+ (+ health-remain (nth dungeon current-spot -1000)) (nth dungeon (+ current-spot 1) -1000)) 0) (recur
                                                                                                                    start-health
                                                                                                                    (+ health-remain (nth dungeon current-spot -1000))
                                                                                                                    (+ current-spot 1)) ; The knight can survive a step right, so it goes right
              (> (+ (+ health-remain (nth dungeon current-spot -1000)) (nth dungeon (+ current-spot col) -1000)) 0) (recur
                                                                                                                      start-health
                                                                                                                      (+ health-remain (nth dungeon current-spot -1000))
                                                                                                                      (+ current-spot col)) ; The knight goes down
              :else (recur (inc start-health) (inc start-health) 0) ; if the knight would die either direction, reset at the top of the dungeon with 1 additional health
              )))
       inputs))

(defn make-dungeon-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-dungeon-error-function
    ([individual]
      (the-actual-dungeon-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-dungeon-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input1 input2 input3] correct-output] (case data-cases
                                                                    :train train-cases
                                                                    :test test-cases
                                                                    [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input3 :input)
                                                     (push-item input2 :input)
                                                     (push-item input1 :input)))
                             result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %2d | Program output: %s" (str correct-output) (str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is absolute distance from correct number
                         (if (number? result)
                           (abs (- result correct-output)) ; distance from correct integer
                           1000000) ; penalty for no return value
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-dungeon-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map dungeon-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def dungeon-train-and-test-cases
  (get-dungeon-train-and-test dungeon-data-domains))

(defn dungeon-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first dungeon-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second dungeon-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn dungeon-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Dungeon problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-dungeon-error-function-from-cases (first dungeon-train-and-test-cases)
                                                                      (second dungeon-train-and-test-cases))
   :atom-generators dungeon-atom-generators
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
   :problem-specific-report dungeon-report
   :problem-specific-initial-report dungeon-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
