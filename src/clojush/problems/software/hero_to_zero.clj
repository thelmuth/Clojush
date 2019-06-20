;; hero_to_zero.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;

(ns clojush.problems.software.hero-to-zero
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def hero-to-zero-atom-generators
  (concat (list
            (tag-instruction-erc [:exec :integer :boolean] 1000)
            (tagged-instruction-erc 1000)
            ;;; end ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec])))

;; A list of data domains for the hero-to-zero problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def hero-to-zero-data-domains
  [#_[(list [1 2]
            [10000 10000]
            [59 3]
            [532 1432]) 4 0]
   [(fn [] (list (inc (rand-int 10000)) (+ (rand-int 10000) 2))) 196 2000]
   ])

;;Can make hero-to-zero test data like this:
;(test-and-train-data-from-domains hero-to-zero-data-domains)

; Helper function for error function
(defn hero-to-zero-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [[input1 input2] output]."
  [inputs]
  (map (fn [[in1 in2]]
          (vector [in1 in2]
            (loop [n in1 k in2 steps 0]
              (cond
                (= n 0) steps
                (= (mod n k) 0) (recur (/ n k) k (inc steps))
                :else (recur (dec n) k (inc steps))))))
       inputs))

(defn make-hero-to-zero-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-hero-to-zero-error-function
    ([individual]
      (the-actual-hero-to-zero-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-hero-to-zero-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input1 input2] out-int] (case data-cases
                                                              :train train-cases
                                                              :test test-cases
                                                              [])]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input2 :input)
                                                     (push-item input1 :input)
                                                     (push-item "" :output)))
                             printed-result (stack-ref :integer 0 final-state)]
                         (when print-outputs
                           (println (format "Correct output: %-19s | Program output: %-19s" (str out-int) printed-result)))
                         ; Record the behavior
                         (swap! behavior conj printed-result)
                         ; Each test case is either right or wrong
                         (if (number? printed-result)
                           (abs (- printed-result out-int)) ;distance from correct integer
                           1000000))))] ; penalty for no return value
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

(defn get-hero-to-zero-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map hero-to-zero-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def hero-to-zero-train-and-test-cases
  (get-hero-to-zero-train-and-test hero-to-zero-data-domains))

(defn hero-to-zero-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first hero-to-zero-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second hero-to-zero-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn hero-to-zero-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Hero to Zero problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-hero-to-zero-error-function-from-cases (first hero-to-zero-train-and-test-cases)
                                                          (second hero-to-zero-train-and-test-cases))
   :atom-generators hero-to-zero-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 200
   :parent-selection :lexicase
   :epigenetic-markers [:close]
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 5
   :uniform-mutation-rate 0.01
   :problem-specific-report hero-to-zero-report
   :problem-specific-initial-report hero-to-zero-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000
   })
