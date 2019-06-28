;; poker.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.poker
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def poker-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char :vector_string] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:boolean :integer :string :char :exec :vector_string])))


;; Define test cases
(defn poker-input
  "Makes a poker input."
  []
  (vector (apply str (clojure.string/join (repeatedly 5 #(str (inc (rand-int 9)) (rand-nth ["h" "c" "s" "d"])))))
          (apply str (clojure.string/join (repeatedly 5 #(str (inc (rand-int 9)) (rand-nth ["h" "c" "s" "d"])))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def poker-data-domains
  [[(list ["1h2h3h4h5h"
           "5h1d8c6s2h"]    ; straight flush
          ["4h4s4c4d9c"
           "8d3h5c5h2c"]    ; four of a kind
          ["7h3c9h3d1c"
           "3h3d3c2s2c"]    ; full house
          ["1h3h5h8h9h"
           "3c6d1s9h2c"]    ; flush
          ["8c8h2c4h6d"
           "1h2c3d4s5h"]    ; straight
          ["3h3c3d8c1s"
           "8h1s4c6d2s"]    ; three of a kind
          ["2h2s8d6h6c"
           "4h1h3s8d7c"]    ; two pair
          ["9s4d5c1h8h"
           "2s5h6c2h9d"]    ; pair
          ["1h2h3h4h5h"
           "1s2s3s4s5s"]    ; high rank tie
          ["4s9h1c6d2s"
           "5s1d9c7d2c"]    ; nothing tie
          ) 10 0]  ;; "Special" inputs covering the base cases
   [(fn [] (poker-input)) 190 2000]
   ])

;;Can make poker test data like this:
;(test-and-train-data-from-domains poker-data-domains)


; Helper function for the below helper function
(defn hand-rank
  "The rank is:
  8 - Straight flush
  7 - Four of a kind
  6 - Full House
  5 - Flush
  4 - Straight
  3 - Three of a kind
  2 - Two Pairs
  1 - Pair
  0 - Nothing "
  [hand-string]
  (let [hand (partition 2 hand-string)] ; creates a list of each individual card (it's a list of characters)
  (cond
    (and (= (count (remove #(not= 5 (val %)) (frequencies (list (nth (nth hand 0) 1)
                                             (nth (nth hand 1) 1)
                                             (nth (nth hand 2) 1)
                                             (nth (nth hand 3) 1)
                                             (nth (nth hand 4) 1))))) 1)
         (let [sort-hand (sort (list (nth (nth hand 0) 0)
                                     (nth (nth hand 1) 0)
                                     (nth (nth hand 2) 0)
                                     (nth (nth hand 3) 0)
                                     (nth (nth hand 4) 0)))]
                  (= (- (int (last sort-hand)) (int (first sort-hand))) 4)) true) 8   ; straight flush
    (= (count (remove #(not= 4 (val %)) (frequencies (list (nth (nth hand 0) 0)
                                        (nth (nth hand 1) 0)
                                        (nth (nth hand 2) 0)
                                        (nth (nth hand 3) 0)
                                        (nth (nth hand 4) 0))))) 1) 7  ; 4 of a kind
    (and (= (count (remove #(not= 3 (val %)) (frequencies (list (nth (nth hand 0) 0)
                                             (nth (nth hand 1) 0)
                                             (nth (nth hand 2) 0)
                                             (nth (nth hand 3) 0)
                                             (nth (nth hand 4) 0))))) 1)
         (= (count (remove #(not= 2 (val %)) (frequencies (list (nth (nth hand 0) 0)
                                             (nth (nth hand 1) 0)
                                             (nth (nth hand 2) 0)
                                             (nth (nth hand 3) 0)
                                             (nth (nth hand 4) 0))))) 1)) 6   ; Full house
    (= (count (remove #(not= 5 (val %)) (frequencies (list (nth (nth hand 0) 1)
                                        (nth (nth hand 1) 1)
                                        (nth (nth hand 2) 1)
                                        (nth (nth hand 3) 1)
                                        (nth (nth hand 4) 1))))) 1) 5   ; flush
    (let [sort-hand (sort (list (nth (nth hand 0) 0)
                                (nth (nth hand 1) 0)
                                (nth (nth hand 2) 0)
                                (nth (nth hand 3) 0)
                                (nth (nth hand 4) 0)))]
             (= (- (int (last sort-hand)) (int (first sort-hand))) 4)) 4 ; straight
    (= (count (remove #(not= 3 (val %)) (frequencies (list (nth (nth hand 0) 0)
                                        (nth (nth hand 1) 0)
                                        (nth (nth hand 2) 0)
                                        (nth (nth hand 3) 0)
                                        (nth (nth hand 4) 0))))) 1) 3 ; three of a kind
    (= (count (remove #(not= 2 (val %)) (frequencies (list (nth (nth hand 0) 0)
                                        (nth (nth hand 1) 0)
                                        (nth (nth hand 2) 0)
                                        (nth (nth hand 3) 0)
                                        (nth (nth hand 4) 0))))) 2) 2 ; two pair
    (= (count (remove #(not= 2 (val %)) (frequencies (list (nth (nth hand 0) 0)
                                        (nth (nth hand 1) 0)
                                        (nth (nth hand 2) 0)
                                        (nth (nth hand 3) 0)
                                        (nth (nth hand 4) 0))))) 1) 1 ; one pair
    :else 0)))

; Helper function for error function
(defn poker-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (cond
              (> (hand-rank (nth in 0)) (hand-rank (nth in 1))) (hand-rank (nth in 0))
              (< (hand-rank (nth in 0)) (hand-rank (nth in 1))) (hand-rank (nth in 1))
              :else -1)))
       inputs))

(defn make-poker-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-poker-error-function
    ([individual]
      (the-actual-poker-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-poker-error-function individual data-cases false))
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
                             result (top-item :integer final-state)]
                         (when print-outputs
                           (println (format "| Correct output: %s\n| Program output: %s\n" correct-output result)))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is difference of integers
                         (if (number? result)
                           (abs (- result correct-output)) ;distance from correct integer
                           100000) ;penalty for no return value
                           )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-poker-train-and-test
  "Returns the train and test cases."
  [data-domains]
    (map poker-test-cases
         (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def poker-train-and-test-cases
  (get-poker-train-and-test poker-data-domains))

(defn poker-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first poker-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second poker-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn poker-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Poker problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-poker-error-function-from-cases (first poker-train-and-test-cases)
                                                               (second poker-train-and-test-cases))
   :atom-generators poker-atom-generators
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
   :problem-specific-report poker-report
   :problem-specific-initial-report poker-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 500000
   })
