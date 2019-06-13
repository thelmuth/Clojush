;; pig_latin.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string of length <= 50 containing lowercase words separated by single
;; spaces, print the string with each word translated to pig Latin. More
;; specifically, if a word starts with a vowel, it should have "ay" added to its
;; end; otherwise, the first letter is moved to the end of the word, followed by "ay".
;;
;; input stack has the input string

(ns clojush.problems.software.pig-latin
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

(defn pig-latin-input
  "Makes a Pig Latin input of length len."
  [len]
  (apply str (interpose \space
                        (remove empty? (string/split (apply str
                                                            (repeatedly len
                                                                        (fn []
                                                                          (if (< (lrand) 0.2)
                                                                            \space
                                                                            (lrand-nth (map char (range 97 123)))))))
                                                     #" ")))))

; Atom generators
(def pig-latin-atom-generators
  (concat (list
            "ay"
            \space
            \a \e \i \o \u
            "aeiou"
            ;;; end constants
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            (fn [] (pig-latin-input (lrand-int 21))) ;String ERC
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec :print])))

(defn make-pig-latin-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-pig-latin-error-function
    ([individual]
      (the-actual-pig-latin-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-pig-latin-error-function individual data-cases false))
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

; Define train and test cases
(def pig-latin-train-and-test-cases
  (map #(sort-by (comp count first) %)
    (train-and-test-cases-from-dataset "pig-latin" 167 1000)))

(defn pig-latin-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first pig-latin-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second pig-latin-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn pig-latin-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Pig Latin problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-pig-latin-error-function-from-cases (first pig-latin-train-and-test-cases)
                                                             (second pig-latin-train-and-test-cases))
   :atom-generators pig-latin-atom-generators
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
   :problem-specific-report pig-latin-report
   :problem-specific-initial-report pig-latin-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
