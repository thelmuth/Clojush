;; tic_tac_toe.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.tic-tac-toe
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def tic-tac-toe-atom-generators
  (concat (list
            "X won"
            "O won"
            "No winner"
            "X"
            "O"
            "."
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char :vector_string :vector_vector_string] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:exec :integer :boolean :string :char :vector_string :vector_vector_string])))


;; Define test cases
(defn tic-tac-toe-input
  "Makes a Tic Tac Toe input."
  [in]
    (case in
        0 "."
        1 "O"
        2 "X"))

(defn make-row
  []
  (vec (repeatedly 3 #(tic-tac-toe-input (lrand-int 3)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def tic-tac-toe-data-domains
  [[(list [["." "." "."]
           ["." "." "."]
           ["." "." "."]]
          [["X" "O" "X"]
           ["O" "X" "O"]
           ["X" "O" "X"]]
          [["O" "X" "X"]
           ["X" "O" "O"]
           ["O" "X" "X"]]
          [["." "." "."]
           ["." "O" "."]
           ["." "." "."]]
          [["X" "." "."]
           ["X" "O" "O"]
           ["X" "O" "."]]
          [["X" "." "."]
           ["X" "O" "O"]
           ["X" "O" "."]]
          [["." "X" "."]
           ["O" "O" "O"]
           ["X" "." "X"]]
          [["X" "." "X"]
           ["X" "X" "O"]
           ["O" "X" "O"]]
          [["X" "." "O"]
           ["X" "O" "."]
           ["O" "." "X"]]
          [["X" "O" "X"]
           ["X" "O" "X"]
           ["O" "X" "O"]]) 10 0]  ;; "Special" inputs covering the base cases
   [(fn [] (vec (repeatedly 3 #(make-row)))) 90 1000]
   ])

;;Can make Tic Tac Toe test data like this:
;(test-and-train-data-from-domains tic-tac-toe-data-domains)

; Helper function for error function
; Cite: Some code from https://codereview.stackexchange.com/questions/62979/clojure-tic-tac-toe-solver
(defn tic-tac-toe-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
              (case
                  (let [rows [(nth in 0)
                              (nth in 1)
                              (nth in 2)]
                        cols (apply map vector rows)
                        diags (map #(map % (range 3)) [#((rows %) %) #((rows %) (- 2 %))])
                        lines (concat rows cols diags)]
                  (first (some (comp #{#{"X"} #{"O"}} set) lines)))
                  "X" "X won"
                  "O" "O won"
                  "No winner")))
       inputs))

(defn make-tic-tac-toe-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-tic-tac-toe-error-function
    ([individual]
      (the-actual-tic-tac-toe-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-tic-tac-toe-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input correct-output] (case data-cases
                                                    :train train-cases
                                                    :test test-cases
                                                    [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input :input)
                                                     (push-item "" :output)))
                             printed-result (stack-ref :output 0 final-state)]
                         (when print-outputs
                           (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str printed-result))))
                         ; Record the behavior
                         (swap! behavior conj printed-result)
                         ; Error is boolean error
                         (if (= printed-result correct-output)
                           0
                           1))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-tic-tac-toe-train-and-test
  "Returns the train and test cases."
  [data-domains]
    (map tic-tac-toe-test-cases
         (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def tic-tac-toe-train-and-test-cases
  (get-tic-tac-toe-train-and-test tic-tac-toe-data-domains))

(defn tic-tac-toe-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first tic-tac-toe-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second tic-tac-toe-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn tic-tac-toe-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Tic Tac Toe problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-tic-tac-toe-error-function-from-cases (first tic-tac-toe-train-and-test-cases)
                                                               (second tic-tac-toe-train-and-test-cases))
   :atom-generators tic-tac-toe-atom-generators
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
   :problem-specific-report tic-tac-toe-report
   :problem-specific-initial-report tic-tac-toe-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
