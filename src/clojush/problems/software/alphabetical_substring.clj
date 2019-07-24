;; alphabetical_substring.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.alphabetical-substring
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def alphabetical-substring-atom-generators
  (concat (list
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec])))

(defn random-string
  [len]
  (apply str (repeatedly len #(rand-nth "abcdefghijklmnopqrstuvwxyz"))))

;; Define test cases
(defn alphabetical-substring-input
  "Makes a Alphabetical Substring input of length len"
  [sorted]
  (if (< sorted 3)
    (str (random-string (rand-int 25)) (apply str (sort (random-string (+ (rand-int 49) 2)))) (random-string 25))
    (random-string (rand-int 100))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def alphabetical-substring-data-domains
  [[(list ""
          "a"
          "abc"
          "abccba"
          "abcabc"
          "azcbobobegghakl"
          "abcdefghijklmnopqrstuvwxyz"
          "zyxwvutsrpqonmlkjihgfedcba"
          ) 8 0]
   [(fn [] (alphabetical-substring-input (lrand-int 4))) 192 2000]
   ])

;;Can make Alphabetical Substring test data like this:
;(test-and-train-data-from-domains alphabetical-substring-data-domains)

(defn str-sorted?
  [string]
  (= string (apply str (sort string))))

; Helper function for error function
(defn alphabetical-substring-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (loop [substring (str (first in)) length 0 current-str in substr-len 1]
              (cond
                (<= (count current-str) 1) substring
                (> substr-len (count current-str)) substring
                (and (str-sorted? (subs current-str 0 substr-len))
                     (> substr-len length)) (recur (subs current-str 0 substr-len) substr-len current-str (inc substr-len))
                :else (recur substring length (apply str (rest current-str)) substr-len)))))
       inputs))

(defn make-alphabetical-substring-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-alphabetical-substring-error-function
    ([individual]
     (the-actual-alphabetical-substring-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-alphabetical-substring-error-function individual data-cases false))
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

(defn get-alphabetical-substring-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map alphabetical-substring-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def alphabetical-substring-train-and-test-cases
  (get-alphabetical-substring-train-and-test alphabetical-substring-data-domains))

(defn alphabetical-substring-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first alphabetical-substring-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second alphabetical-substring-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn alphabetical-substring-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Alphabetical Substring problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-alphabetical-substring-error-function-from-cases (first alphabetical-substring-train-and-test-cases)
                                                                  (second alphabetical-substring-train-and-test-cases))
   :atom-generators alphabetical-substring-atom-generators
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
   :problem-specific-report alphabetical-substring-report
   :problem-specific-initial-report alphabetical-substring-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
