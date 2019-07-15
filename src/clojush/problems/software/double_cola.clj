;; double_cola.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.double-cola
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def double-cola-atom-generators
  (concat (list
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec :vector_string])))

(defn name-generator
  []
  (apply str (map char (repeatedly 5 #(+ (rand-int 26) 97)))))

;; Define test cases
(defn double-cola-input
  "Makes a Double Cola input vector"
  []
  (vector
    (vec (repeatedly (inc (rand-int 20)) name-generator))
    (inc (rand-int 1000))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def double-cola-data-domains
  [[(list [["asdfg"] 1000]
          [["asdfg" "qwert" "opiyt" "jklgh"] 1]
          [["qjryx" "hrvmc" "xdsyn" "nwuod" "wpxxm" "rdyuj" "vqvrk" "kwcxw" "ccydx" "ffhza" "hcnid" "hfllb" "evshk" "wiptl" "xoscc" "zmrdw" "tnpob" "wfjel" "asdfg" "qwert"] 7000]
          ) 3 0]
   [(fn [] (double-cola-input)) 197 2000] ;; Random length
   ])

;;Can make Double Cola test data like this:
;(test-and-train-data-from-domains double-cola-data-domains)

; Helper function for error function
(defn double-cola-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [[in1 in2]]
          (vector [in1 in2]
            (loop [num-left in2 people in1]
              (if (= num-left 1) (first people)
                  (recur (dec num-left) (conj (vec (rest people)) (first people) (first people)))))))
       inputs))

(defn make-double-cola-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-double-cola-error-function
    ([individual]
     (the-actual-double-cola-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-double-cola-error-function individual data-cases false))
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
                           result (top-item :string final-state)]
                       (when print-outputs
                           (println (format "Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is levenshtein distance
                         (levenshtein-distance correct-output (str result))
                         )))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-double-cola-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map double-cola-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def double-cola-train-and-test-cases
  (get-double-cola-train-and-test double-cola-data-domains))

(defn double-cola-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first double-cola-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second double-cola-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn double-cola-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Double Cola problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-double-cola-error-function-from-cases (first double-cola-train-and-test-cases)
                                                                  (second double-cola-train-and-test-cases))
   :atom-generators double-cola-atom-generators
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
   :problem-specific-report double-cola-report
   :problem-specific-initial-report double-cola-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
