;; number_io.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; This problem file defines the following problem:
;; There are two inputs, a float and an int. The program must read them in,
;; find their sum as a float, and print the result as a float.
;;
;; NOTE: input stack: in1 (float), in2 (int)
;;       output stack: printed output

(ns clojush.problems.software.number-io
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def num-io-atom-generators
  (concat (list
            (fn [] (- (lrand-int 201) 100))
            (fn [] (- (* (lrand) 200) 100.0))
            (tag-instruction-erc [:float :integer] 1000)
            (tagged-instruction-erc 1000)
            ;;; end ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (registered-for-stacks [:float :integer :print])))

(defn make-number-io-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-num-io-error-function
    ([individual]
      (the-actual-num-io-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-num-io-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq
                       [[case-num [input1 in-int correct-output]] (map-indexed vector (case data-cases
                                                                                     :train train-cases
                                                                                     :test test-cases
                                                                                     []))]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item in-int :input)
                                                      (push-item input1 :input)
                                                      (push-item "" :output)))
                           result (stack-ref :output 0 final-state)]


;                       (println result)
;                       (println (type result))
                                        ; print if wrong answer
                       (when (not= (subs (str result) 0 7)
                                   (subs (pr-str (round-to-n-decimal-places correct-output 10)) 0 7))
                         (println "############################################################")
                         (println "Wrong result:" input1 "||" correct-output result)
                         (println "############################################################"))
                                        ; print case numbers sometimes
                       (when (or (= (mod case-num 10000) 9999)
                                 (= (mod case-num 10000) 1))
                         (prn "At case" case-num ", input =", input1))  


                       ))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def number-io-train-and-test-cases
  (train-and-test-cases-from-dataset "number-io" 0 100000000))

(defn number-io-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first number-io-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second number-io-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn num-io-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Number IO problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 1.0E-4)
      (doseq [[i [num-error lev-dist]] (map vector
                                            (range)
                                            (partition 2 best-test-errors))]
        (println (format "Test Case  %3d | Numeric Error: %19.14f | Levenshtein Distance: %d" i (float num-error) lev-dist))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-number-io-error-function-from-cases (first number-io-train-and-test-cases)
                                                             (second number-io-train-and-test-cases))
   :atom-generators num-io-atom-generators
   :max-points 800
   :max-genome-size-in-initial-program 100
   :evalpush-limit 200
   :population-size 1000
   :max-generations 200
   :parent-selection :lexicase
   :epigenetic-markers []
   :genetic-operator-probabilities {:alternation 0.3
                                    :uniform-mutation 0.2
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 5
   :uniform-mutation-rate 0.01
   :problem-specific-report num-io-report
   :problem-specific-initial-report number-io-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   :error-threshold 1.0E-4
   })



;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-max-points 800)

(reset! global-evalpush-limit 200)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
(def tom-program
  '(in2 float_frominteger in1 float_add print_float))

#_(def tom-program-BAD
  '(integer_stackdepth integer_stackdepth exec_do*range
                       (exec_do*vector_integer in1) integer_dup 42 exec_do*times
                       (integer_mod integer_swap) integer_inc exec_do*times integer_lte
                       vector_integer_dup_times integer_stackdepth))

;; This program is hand-written
#_(def tom-program
 '(
    4 in1 integer_lt
    exec_when
    (
      4 print_integer
      4 integer_dup integer_dup integer_mult integer_dup in1 integer_lt
      exec_while
      (
        print_newline print_integer 
        integer_inc integer_inc
        integer_dup integer_dup integer_mult integer_dup in1 integer_lt
        )
      )
    ))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind false)

(test-program-on-testing tom-ind false)



