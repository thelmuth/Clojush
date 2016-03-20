;; double_letters.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string, print the string, doubling every letter character, and
;; trippling every exclamation point. All other non-alphabetic and non-exclamation
;; characters should be printed a single time each. The input string will have
;; maximum length of 20 characters.
;;
;; input stack has the input string

(ns clojush.problems.software.double-letters
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def double-letters-atom-generators
  (concat (list
            \!
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))


;; Define test cases
(defn double-letters-input
  "Makes a Double Letters input of length len."
  [len]
  (apply str
         (repeatedly len
                     #(lrand-nth (concat [\newline \tab]
                                         (map char (range 32 127)))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def double-letters-data-domains
  [[(list "", "A", "!", " ", "*", "\t", "\n", "B\n", "\n\n", "CD", "ef", "!!", "q!", "!R", "!#", "@!", "!F!", "T$L", "4ps", "q\t ", "!!!"
          (apply str (take 13 (cycle (list \i \: \!))))
          (apply str (repeat 20 \8))
          (apply str (repeat 20 \space))
          (apply str (repeat 20 \s))
          (apply str (repeat 20 \!))
          (apply str (take 20 (cycle (list \H \a \space))))
          (apply str (take 20 (cycle (list \x \newline \y \!))))
          (apply str (take 20 (cycle (list \1 \!))))
          (apply str (take 20 (cycle (list \G \5))))
          (apply str (take 20 (cycle (list \> \_ \= \]))))
          (apply str (take 20 (cycle (list \k \! \!))))) 32 0] ;; "Special" inputs covering some base cases
   [(fn [] (double-letters-input (inc (lrand-int 20)))) 68 1000]
   ])

;;Can make Double Letters test data like this:
;(test-and-train-data-from-domains double-letters-data-domains)

; Helper function for error function
(defn double-letters-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (apply str (flatten (map #(cond
                                             (Character/isLetter %) (list % %)
                                             (= % \!) (list % % %)
                                             :else %)
                                          in)))))
       inputs))

(def train-and-test-cases (atom ()))

; Define error function. For now, each run uses different random inputs
(defn double-letters-error-function
  "Returns the error function for the Double Letters problem. Takes as
   input Double Letters data domains."
  [data-domains]
  (let [[train-cases test-cases] (map #(sort-by (comp count first) %)
                                      (map double-letters-test-cases
                                           (test-and-train-data-from-domains data-domains)))]
    (when true ;; This one stores the test cases to later print to a file
      (reset! train-and-test-cases [train-cases test-cases]))
    (when true ;; Change to false to not print test cases
      (doseq [[i case] (map vector (range) train-cases)]
        (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
      (doseq [[i case] (map vector (range) test-cases)]
        (println (format "Test Case: %3d | Input/Output: %s" i (str case)))))
    (fn the-actual-double-letters-error-function
      ([program]
        (the-actual-double-letters-error-function program :train))
      ([program data-cases] ;; data-cases should be :train or :test
        (the-actual-double-letters-error-function program data-cases false))
      ([program data-cases print-outputs]
        (let [behavior (atom '())
              errors (doall
                       (for [[input correct-output] (case data-cases
                                                      :train train-cases
                                                      :test test-cases
                                                      [])]
                         (let [final-state (run-push program
                                                     (->> (make-push-state)
                                                       (push-item input :input)
                                                       (push-item "" :output)))
                               printed-result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str printed-result))))
                           ; Record the behavior
                           (when @global-print-behavioral-diversity
                             (swap! behavior conj printed-result))
                           ; Error is Levenshtein distance
                           (levenshtein-distance correct-output printed-result))))]
          (when @global-print-behavioral-diversity
            (swap! population-behaviors conj @behavior))
          errors)))))

(def error-diversities (atom ()))

(defn double-letters-report
  "Custom generational report."
  [best population generation error-function report-simplifications
   {:keys [run-name run-log-directory] :as argmap}]
  (let [best-program (not-lazy (:program best))
        best-test-errors (error-function best-program :test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Double Letters problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best-program :train true)
    (println ";;******************************")
    (if (not-empty population) ; Make sure this isn't during simplification, which has an empty population
      (let [error-diversity (float (/ (count (frequencies (map :errors population))) (count population)))
            max-diff-last-10 (apply max
                                    (conj (map #(- % error-diversity)
                                               (take 10 @error-diversities))
                                          -2999 ; large negative number to make sure list isn't empty for generation 0
                                          ))]
        (swap! error-diversities conj error-diversity)
        (when (and (> generation 10)
                   (> max-diff-last-10 0.25))
          (spit (str run-log-directory run-name "_population.dat")
                (pr-str population))
          (spit (str run-log-directory run-name "_train_and_test_cases.dat")
                (pr-str @train-and-test-cases))
          ;; To later load train and test cases, do something like: (read-string (slurp "/Users/helmuth/Documents/Clojure/Results/testing-logs/run_default-76a2b63d-6a7e-4ab7-8e01-443e96cdf858_train_and_test_cases.dat"))
          (assoc best :success true))))
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (double-letters-error-function double-letters-data-domains)
   :atom-generators double-letters-atom-generators
   :max-points 1600
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
   :problem-specific-report double-letters-report
   :print-behavioral-diversity true
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
