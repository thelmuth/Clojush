;; digits.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given an integer |x| < 10,000,000,000, print that integer's digits each on
;; their own line starting with the least significant digit. A negative integer
;; should have the negative sign printed before the most significant digit.
;;
;; input stack has input integer

(ns clojush.problems.software.digits
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

; Atom generators
(def digits-atom-generators
  (concat (list
            \newline
            ;;; end constants
            (fn [] (- (lrand-int 21) 10))
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :string :char :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))

(defn make-digits-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-digits-error-function
    ([individual]
      (the-actual-digits-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-digits-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq
                       [[case-num [input1 correct-output]] (map-indexed vector
                                                             (case data-cases
                                                               :train train-cases
                                                               :test test-cases
                                                               []))]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)
                                                      (push-item "" :output)))
                           result (stack-ref :output 0 final-state)]
                       (when print-outputs
                         (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))

                       
                       ; print if wrong answer
                       (when (not= result correct-output)
                         (println "############################################################")
                         (println "Wrong result:" input1 correct-output result)
                         (println "############################################################"))
                       ; print case numbers sometimes
                       (when (or (= (mod case-num 10000) 9999)
                                 (= (mod case-num 10000) 1))
                         (prn "At case" case-num ", input=", input1))  

                       
                                        ; Error is Levenshtein distance of printed strings
))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def digits-train-and-test-cases
  (train-and-test-cases-from-dataset "digits" 0 10000000000))

(defn digits-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first digits-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second digits-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn digits-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Digits problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-digits-error-function-from-cases (first digits-train-and-test-cases)
                                                          (second digits-train-and-test-cases))
   :atom-generators digits-atom-generators
   :max-points 1200
   :max-genome-size-in-initial-program 150
   :evalpush-limit 600
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
   :problem-specific-report digits-report
   :problem-specific-initial-report digits-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })


;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-evalpush-limit 600)

(reset! global-max-points 1200)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
(def tom-program
  '(boolean_stackdepth exec_s in1 char_stackdepth
                       (exec_when (integer_gte in1 string_butlast integer_sub string_frominteger))
                       exec_string_iterate
                       (string_first \newline) char_isdigit exec_y print_char))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind  false)

(test-program-on-testing tom-ind false)




