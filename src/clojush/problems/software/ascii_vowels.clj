;; ascii_vowels.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.ascii-vowels
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def ascii-vowels-atom-generators
  (concat (list
            97
            101
            105
            111
            117
            121
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec])))

; Helper function
(defn vowel
  [char]
  (if (or
        (= char 97)     ; a
        (= char 101)    ; e
        (= char 105)    ; i
        (= char 111)    ; o
        (= char 117)    ; u
        (= char 121)    ; y
        ) true false))

;; Define test cases
(defn ascii-vowels-input
  "Makes a Ascii Vowels input vector"
  [use-vowel]
  (if use-vowel
    (let [vec-len (inc (rand-int 50))
          vowel-num (- vec-len (rand-int vec-len))
          vowels (vec (repeatedly vowel-num #(rand-nth '(97 101 105 111 117 121))))
          others (vec (repeatedly (- vec-len vowel-num) #(rand-int 200)))]
          (vec (flatten (shuffle (conj vowels others)))))
    (vec (map #(if (vowel %) (inc %) %) (repeatedly (inc (rand-int 50)) #(rand-int 200))))
    ))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def ascii-vowels-data-domains
  [[(list []
          [0]
          [97]
          [97 101 105 111 117 121]
          [121]
          [95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125]
          [0 1 2 3 4 5 6 7 8 9 10]
          ) 7 0]
   [(fn [] (ascii-vowels-input true)) 171 1750] ;; True (7/8)
   [(fn [] (ascii-vowels-input false)) 22 250]  ;; False (1/8)
   ])

;;Can make Ascii Vowels test data like this:
;(test-and-train-data-from-domains ascii-vowels-data-domains)

; Helper function for error function
(defn ascii-vowels-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (count (filter true? (map vowel in)))))
       inputs))

(defn make-ascii-vowels-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-ascii-vowels-error-function
    ([individual]
     (the-actual-ascii-vowels-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-ascii-vowels-error-function individual data-cases false))
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
                           (println (format "Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is integer difference
                       (if (number? result)
                         (abs (- result correct-output)) ;distance from correct integer
                         1000000))))] ; penalty for no return value
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-ascii-vowels-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map ascii-vowels-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def ascii-vowels-train-and-test-cases
  (get-ascii-vowels-train-and-test ascii-vowels-data-domains))

(defn ascii-vowels-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first ascii-vowels-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second ascii-vowels-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn ascii-vowels-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Ascii Vowels problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-ascii-vowels-error-function-from-cases (first ascii-vowels-train-and-test-cases)
                                                                  (second ascii-vowels-train-and-test-cases))
   :atom-generators ascii-vowels-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
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
   :problem-specific-report ascii-vowels-report
   :problem-specific-initial-report ascii-vowels-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
