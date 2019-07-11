;; dna.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Given a vector of integers, return a sorted version of the vector
;;
;;

(ns clojush.problems.software.dna
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def dna-atom-generators
  (concat (list
            \A
            \T
            \C
            \G
            "ATCG"
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec])))

;; Define test cases
(defn dna-input
  "Makes a DNA input vector of length len."
  [len]
  (apply str (repeatedly len #(rand-nth (list \A \T \C \G)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def dna-data-domains
  [[(list ""
          "A"
          "T"
          "C"
          "G"
          "ATCG"
          "AT"
          "CG"
          "ACTAGCAGCTAGCATCGATCGATATAGCGGATTATCGAGCGATTCATGCGACTATCTGAGCATCGTACGATCCAGCATGCATCGTGCAATCGAGCTAGCT"
          ) 9 0]
   [(fn [] (dna-input (inc (lrand-int 100)))) 191 2000] ;; Random length
   ])

;;Can make DNA test data like this:
;(test-and-train-data-from-domains dna-data-domains)

; Helper function for below function
(defn swap
  [char]
  (cond
    (= char \A) \T
    (= char \T) \A
    (= char \C) \G
    :else \C))

; Helper function for error function
(defn dna-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (loop [original in new ""]
              (if (= (count original) 0)
                new
                (recur (rest original) (str new (swap (first original))))))))
       inputs))

(defn make-dna-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-dna-error-function
    ([individual]
     (the-actual-dna-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-dna-error-function individual data-cases false))
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
                       ; Error is levenshtein distance, with an added error for each letter that is not ACGT
                       (+
                         (levenshtein-distance correct-output (str result))
                         (reduce + (map #(if (clojure.string/includes? "ATCG" (str (first %))) 0 (second %)) (frequencies (str result))))
                         ))))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-dna-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map dna-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def dna-train-and-test-cases
  (get-dna-train-and-test dna-data-domains))

(defn dna-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first dna-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second dna-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn dna-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- DNA problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-dna-error-function-from-cases (first dna-train-and-test-cases)
                                                                  (second dna-train-and-test-cases))
   :atom-generators dna-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 1000
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
   :problem-specific-report dna-report
   :problem-specific-initial-report dna-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
