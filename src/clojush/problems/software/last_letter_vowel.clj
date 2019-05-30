;; last_letter_vowel.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Return true if last letter in input string of lower case letters is vowel, false otherwise.
;;
;; input stack has the input string

(ns clojush.problems.software.last_letter_vowel
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

;; Define test cases
(defn llv-input
  "Makes a Last Letter Vowel input of length len."
  [len]
  (apply str
         (repeatedly len
                     #(if (< (lrand) 0.2)
                        (lrand-nth "aeiouy")
                        (lrand-nth (map char (concat (range 97 122) (range 97 122))))))))

; Atom generators
(def llv-atom-generators
  (concat (list
            \a
            \e
            \i
            \o
            \u
            \y
            "aeiouy"
            ;;; end constants
            (fn [] (lrand-nth (map char (range 97 122)))) ;Visible character ERC
            (fn [] (llv-input (lrand-int 21))) ;String ERC
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def llv-data-domains
  [[(list "", "a", "v", "o", "aei", "ouy", "chf", "quite", "arej", "asdf", "p"
          "youaremanyyayyea"
          (apply str (repeat 20 \s))
          (apply str (repeat 20 \o))
          (apply str (take 20 (cycle (list \w \i))))
          (apply str (take 20 (cycle (list \x \y))))
          (apply str (take 20 (cycle (list \e \i \o \y))))) 17 0] ;; Hand-chosen edge cases
   [(fn [] (llv-input (inc (lrand-int 20)))) 83 1000]
   ])

;;Can make llv test data like this:
;(test-and-train-data-from-domains llv-data-domains)

; Helper function for error function
(defn llv-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                  (string/includes? "aeiouy" (str (last in)))))
                ;  (string/includes? "aeiouy" (str (last (filter #(some #{%} "aeiouy") in))))))
       inputs))

(defn make-llv-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-llv-error-function
    ([individual]
      (the-actual-llv-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-llv-error-function individual data-cases false))
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
                               result (top-item :boolean final-state)]
                           (when print-outputs
                             (println (format "\n| Correct output: %s\n| Program output: %s" correct-output (str result))))
                           ; Record the behavior
                           (swap! behavior conj result)
                           ; Error is Levenshtein distance and, if ends in an integer, distance from correct integer
                           (if (= result correct-output)
                             0
                             1))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-llv-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map llv-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def llv-train-and-test-cases
  (get-llv-train-and-test llv-data-domains))

(defn llv-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first llv-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second llv-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn llv-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Last Letter Vowel problem report - generation %s\n" generation)(flush)
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
 {:error-function (make-llv-error-function-from-cases (first llv-train-and-test-cases)
                                                            (second llv-train-and-test-cases))
  :atom-generators llv-atom-generators
  :max-points 3200
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
  :problem-specific-report llv-report
  :problem-specific-initial-report llv-initial-report
  :report-simplifications 0
  :final-report-simplifications 5000
  :max-error 1
  })
