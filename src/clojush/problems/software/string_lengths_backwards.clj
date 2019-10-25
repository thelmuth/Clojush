;; string_lengths_backwards.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a vector of strings with length <= 50, where each string has
;; length <= 50, print the length of each string in the vector starting with
;; the last and ending with the first.
;;
;; input stack has 1 input vector of strings

(ns clojush.problems.software.string-lengths-backwards
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def string-lengths-atom-generators
  (concat (list
            (fn [] (- (lrand-int 201) 100)) ; Integer ERC in [-100,100]
            ;;; end ERCs
            (tag-instruction-erc [:string :vector_string :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :vector_string :integer :boolean :exec :print])))

(defn make-string-lengths-backwards-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-string-lengths-error-function
    ([individual]
      (the-actual-string-lengths-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-string-lengths-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq
                       [[case-num [input1 correct-output]] (map-indexed vector (case data-cases
                                                                                 :train train-cases
                                                                                 :test test-cases
                                                                                 []))]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)
                                                      (push-item "" :output)))
                           result (stack-ref :output 0 final-state)]

                       
                                        ; print if wrong answer
                       (when (not= result correct-output)
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
(def string-lengths-backwards-train-and-test-cases
  (train-and-test-cases-from-dataset "string-lengths-backwards" 0 10000000))

(defn string-lengths-backwards-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first string-lengths-backwards-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second string-lengths-backwards-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn string-lengths-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- String Lengths problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-string-lengths-backwards-error-function-from-cases (first string-lengths-backwards-train-and-test-cases)
                                                                            (second string-lengths-backwards-train-and-test-cases))
   :atom-generators string-lengths-atom-generators
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
   :problem-specific-report string-lengths-report
   :problem-specific-initial-report string-lengths-backwards-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })


;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-max-points 1200)

(reset! global-evalpush-limit 600)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
(def tom-program
  '(in1 vector_string_reverse vector_string_pushall exec_s vector_string_dup_items (string_length print_integer string_empty exec_while integer_empty print_newline) exec_dup))


;; This program is hand-written
#_(def tom-program
  '(
     in1 string_length string_length integer_min ;get length of shorter string
     integer_dup 0 integer_lte exec_when exec_flush ;when shorter string has length <= 0, don't do anything
     exec_do*count
     
     ))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind false)

;(test-program-on-testing tom-ind false)





