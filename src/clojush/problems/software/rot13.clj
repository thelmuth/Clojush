;; rot13.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.rot13
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def rot13-atom-generators
  (concat (list
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :char :string :print])))

;; Define test cases
(defn rot13-input
  "Makes a Rot13 input vector of length len."
  [len]
  (apply str (map char (repeatedly len #(+ (rand-int 26) 97)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def rot13-data-domains
  [[(list ""
          "a"
          "z"
          "thislongtestcontainsexactlyonehundredcharacterswhichisthelimitforthisprogramanditwillkeepgoingwaitno"
          "thequickbrownfoxjumpsoverthelazydog"
          "saaaaaaaaaaaaaaaaaaaaaaaaaaaaame"
          "abcdefghijklmnopqrstuvwxyz") 7 0] ;; "Special" inputs covering most base cases.
   [(fn [] (rot13-input (inc (lrand-int 100)))) 193 2000]
   ])

;;Can make rot13 test data like this:
;(test-and-train-data-from-domains rot13-data-domains)

; Helper function for error function
(defn rot13-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
           (apply str (map char (map #(if (<= (+ 13 (int %)) 122)
                                                (+ 13 (int %))
                                                (+ 96 (mod (+ 13 (int %)) 122))) in)))))
       inputs))

(defn make-rot13-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-rot13-error-function
    ([individual]
      (the-actual-rot13-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-rot13-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[input1 correct-output] (case data-cases
                                                     :train train-cases
                                                     :test test-cases
                                                     [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input1 :input)
                                                     (push-item "" :output)))
                             result (stack-ref :output 0 final-state)]
                         (when print-outputs
                           (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                         ; Record the behavior
                         (swap! behavior conj result)
                         ; Error is Levenshtein distance of printed strings
                         (levenshtein-distance correct-output result))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-rot13-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map sort (map rot13-test-cases
                 (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def rot13-train-and-test-cases
  (get-rot13-train-and-test rot13-data-domains))

(defn rot13-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first rot13-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second rot13-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn rot13-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Rot13 problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-rot13-error-function-from-cases (first rot13-train-and-test-cases)
                                                                  (second rot13-train-and-test-cases))
   :atom-generators rot13-atom-generators
   :max-points 800
   :max-genome-size-in-initial-program 100
   :evalpush-limit 300
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 5
   :uniform-mutation-rate 0.01
   :problem-specific-report rot13-report
   :problem-specific-initial-report rot13-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
