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
            (tag-instruction-erc [:integer :vector_integer :vector_vector_integer :exec :boolean] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :vector_integer :vector_vector_integer :exec :boolean])))

;; Define test cases
(defn dungeon-input
  "Makes a Dungeon input given a row and col"
  [col]
  (vec (repeatedly col #(- (lrand-int 201) 100))))

(def dungeon-data-domains
  [[(list [[0 0 0]
           [0 0 0]
           [0 0 0]]
          [[-5]]
          [[-23 -54 -34]
           [-65 -78 -96]
           [-32 -21 -43]]
          [[43 45 12]
           [76 87 2]
           [48 98 1]]
          [[-100 -100]
           [100 100]]
          [[100 -50]
           [-50 -49]]) 6 0]
   [(fn [] (let [row (inc (lrand-int 20)) col (inc (lrand-int 20))]
              (vec (repeatedly row #(dungeon-input col))))) 194 2000]
  ])

;;Can make dungeon test data like this:
;(test-and-train-data-from-domains dungeon-data-domains)

(defn old-dungeon-solver-broken
  "Takes a dungeon and returns the health needed.
   Peter's old version that doesn't work on some cases"
  [dungeon]
  (loop [start-health 1 health-remain 1 current-row 0 current-col 0]
    (cond
      (<= (+ health-remain (nth (nth dungeon current-row) current-col)) 0) (recur (inc start-health) (inc start-health) 0 0) ; if the current spot kills you, then restart
      (= (* (inc current-row) (inc current-col)) (* (count dungeon) (count (first dungeon)))) (if (<= (+ health-remain (nth (nth dungeon current-row) current-col)) 0) ; if the knight made it to the end, check if the final spot kills him
                                                                                                (recur (inc start-health) (inc start-health) 0 0)
                                                                                                start-health) ; if it does, restart, otherwise, return the health
      (and (> (+ (+ health-remain (nth (nth dungeon current-row) current-col)) (nth (nth dungeon current-row [-1000]) (inc current-col) -1000)) 0)  ; if the knight could survive both directions
           (> (+ (+ health-remain (nth (nth dungeon current-row) current-col)) (nth (nth dungeon (+ current-row 1) [-1000]) current-col -1000)) 0))
      (if (> (+ (+ health-remain (nth (nth dungeon current-row) current-col)) (nth (nth dungeon current-row [-1000]) (inc current-col) -1000))
             (+ (+ health-remain (nth (nth dungeon current-row) current-col)) (nth (nth dungeon (+ current-row 1) [-1000]) current-col -1000)))
        (recur   ; If going right results in having more health, go right
         start-health
         (+ health-remain (nth (nth dungeon current-row) current-col))
         current-row
         (+ current-col 1))
        (recur  ; otherwise, go down
         start-health
         (+ health-remain (nth (nth dungeon current-row) current-col))
         (+ current-row 1)
         current-col))
      (> (+ (+ health-remain (nth (nth dungeon current-row) current-col)) (nth (nth dungeon current-row [-1000]) (inc current-col) -1000)) 0)
      (recur
       start-health
       (+ health-remain (nth (nth dungeon current-row) current-col))
       current-row
       (+ current-col 1)) ; The knight can survive a step right, so it goes right
      (> (+ (+ health-remain (nth (nth dungeon current-row) current-col)) (nth (nth dungeon (+ current-row 1) [-1000]) current-col -1000)) 0)
      (recur
       start-health
       (+ health-remain (nth (nth dungeon current-row) current-col))
       (+ current-row 1)
       current-col) ; The knight goes down
      :else (recur (inc start-health) (inc start-health) 0 0) ; if the knight would die either direction, reset at the top of the dungeon with 1 additional health
      )))

; Solves the dungeon; memoized!
(def dungeon-solver-helper
  (memoize
   (fn [dungeon row col]
     (let [rows (dec (count dungeon)) ; Last value for rows
           cols (dec (count (first dungeon))) ; Last value for cols
           x (nth (nth dungeon row) col)]
       (if
        (and (= row rows)  ; Base case: If at last cell, return 1 if positive and 1-x if negative
             (= col cols)) (max 1 (- 1 x))
        (let [better-neighbor (cond ; Recursive cases: Find which neighbor is better
                                (= row rows) (dungeon-solver-helper dungeon row (inc col))
                                (= col cols) (dungeon-solver-helper dungeon (inc row) col)
                                :else (min (dungeon-solver-helper dungeon row (inc col))
                                           (dungeon-solver-helper dungeon (inc row) col)))]
          (max 1 (- better-neighbor x))))))))

(defn dungeon-solver
  "Takes a dungeon and returns the health needed."
  [dungeon]
  (dungeon-solver-helper dungeon 0 0))

(def simple-dungeon [[-5 -15 0]
                     [-10 -1000 0]
                     [-100 -100 0]])
;; Best at each cell:
;; ((21 16 1) 
;;  (211 1001 1) 
;;  (201 101 1))

(def simple-dungeon2 [[-5 -15 1000]
                     [10 -1000 -500]
                     [-100 -100 0]])
;; Best at each cell:
;; ((21 16 1)
;;  (191 1101 501)
;;  (201 101 1))

#_(def random-dungeon (let [row 19 col 20]
                      (vec (repeatedly row #(dungeon-input col)))))

; Solves the dungeon
#_(dungeon-solver simple-dungeon)

; Gives the best value at each cell
#_(for [x (range 3)]
  (for [y (range 3)]
    (dungeon-solver-helper simple-dungeon x y)))

; Helper function for error function
(defn dungeon-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input1 output]."
  [inputs]
  (map (fn [dungeon]
         (vector dungeon
                 (dungeon-solver dungeon)))
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
                     (for [[input1 correct-output] (case data-cases
                                                                    :train train-cases
                                                                    :test test-cases
                                                                    [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input1 :input)))
                             result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "Correct output: %2d | Program output: %s" correct-output result)))
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
   :problem-specific-report dungeon-report
   :problem-specific-initial-report dungeon-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
