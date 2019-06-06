;; word_search.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;; Problem Source: https://leetcode.com/problems/word-search/
;;

(ns clojush.problems.software.word-search
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

;; Define test cases
(defn word-search-input
  "Makes a word search input of length len."
  [row col]
  (apply str (repeatedly (* row col) #(char (+ 97 (lrand-int 26))))))

; Atom generators
(def word-search-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            'in2
            'in3
            'in4
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def word-search-data-domains
  [[(list ["" "" 0 0]
          ["a" "a" 1 1]
          ["b" "a" 1 1]
          ["test" "test" 2 2]
          ["test" "test" 1 4]
          ["nothere" "amvkelavi" 3 3]
          ["hello" "shellopeuakvldqapdkflgjelabz" 4 7]
          ["hello" "asdhpeovmnelskquelieovnclqsudouozjb" 5 7]) 8 0] ;; "Special" inputs covering some base cases
   [(fn [] (word-search-input (+ 3 (lrand-int 48)))) 192 2000]
   ])

; (def word-search-data-domains
;   [[(list "", "a", "b", "c", "d", "e", "i", "m", "o", "u", "y", "z"
;           "hello", "there", "world", "eat", "apple", "yellow", "orange", "umbrella", "ouch", "in",
;           "hello there world"
;           "out at the plate"
;           "nap time on planets"
;           "supercalifragilistic"
;           "expialidocious"
;           (apply str (repeat 50 \u))
;           (apply str (repeat 50 \s))
;           (apply str (take 49 (cycle (list \w \space))))
;           (apply str (take 49 (cycle (list \e \space))))
;           (apply str (take 50 (cycle (list \h \a \space))))
;           (apply str (take 49 (cycle (list \x \space \y \space))))) 33 0] ;; "Special" inputs covering some base cases
;    [(fn [] (word-search-input (+ 3 (lrand-int 48)))) 167 1000]
;    ])

;;Can make Word Search test data like this:
;(test-and-train-data-from-domains word-search-data-domains)

; Helper function for error function
(defn word-search-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (apply str (interpose \space
                                       (map #(if (some #{(first %)} "aeiou")
                                               (str % "ay")
                                               (str (apply str (rest %)) (first %) "ay"))
                                           (remove empty? (string/split in #" ")))))))
       inputs))

(defn make-word-search-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-word-search-error-function
    ([individual]
      (the-actual-word-search-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-word-search-error-function individual data-cases false))
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
                             result (stack-ref :output 0 final-state)]
                         (when print-outputs
                           (println (format "\n| Correct output: %s\n| Program output: %s" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is Levenshtein distance for printed string
                         (levenshtein-distance correct-output result)
                         )))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-word-search-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map word-search-test-cases
            (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def word-search-train-and-test-cases
  (get-word-search-train-and-test word-search-data-domains))

(defn word-search-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first word-search-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second word-search-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn word-search-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Word Search problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-word-search-error-function-from-cases (first word-search-train-and-test-cases)
                                                             (second word-search-train-and-test-cases))
   :atom-generators word-search-atom-generators
   :max-points 4000
   :max-genome-size-in-initial-program 500
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
   :problem-specific-report word-search-report
   :problem-specific-initial-report word-search-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
