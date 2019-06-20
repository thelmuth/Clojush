;; clock_in_a_mirror.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.clock-in-a-mirror
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def clock-in-a-mirror-atom-generators
  (concat (list
            "12:00"
            "06:30"
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))


;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def clock-in-a-mirror-data-domains
  [[(list "12:00"
          "01:00"
          "11:59"
          "06:30"
          "12:30"
          "09:45"
          "05:25"
          "01:50"
          "12:01") 9 0] ;; "Special" inputs covering some base cases
   [(fn [] (format "%02d:%02d" (inc (rand-int 12)) (rand-int 60))) 191 2000]
   ])

;;Can make Clock in a Mirror test data like this:
;(test-and-train-data-from-domains clock-in-a-mirror-data-domains)

; Helper function for error function
(defn clock-in-a-mirror-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
           (let [hour (Integer/parseInt (subs in 0 2)) min (Integer/parseInt (subs in 3))]
             (format "%02d:%02d"
                     (if (= hour 12) 12 (- 12 hour))
                     (if (= min 0) 0 (- 60 min))))))
       inputs))

(defn make-clock-in-a-mirror-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-clock-in-a-mirror-error-function
    ([individual]
      (the-actual-clock-in-a-mirror-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-clock-in-a-mirror-error-function individual data-cases false))
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
                         ; Error is Levenshtein distance
                         (levenshtein-distance correct-output printed-result))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-clock-in-a-mirror-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map clock-in-a-mirror-test-cases
      (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def clock-in-a-mirror-train-and-test-cases
  (get-clock-in-a-mirror-train-and-test clock-in-a-mirror-data-domains))

(defn clock-in-a-mirror-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first clock-in-a-mirror-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second clock-in-a-mirror-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn clock-in-a-mirror-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Clock in a Mirror problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-clock-in-a-mirror-error-function-from-cases (first clock-in-a-mirror-train-and-test-cases)
                                                                  (second clock-in-a-mirror-train-and-test-cases))
   :atom-generators clock-in-a-mirror-atom-generators
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
   :problem-specific-report clock-in-a-mirror-report
   :problem-specific-initial-report clock-in-a-mirror-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
