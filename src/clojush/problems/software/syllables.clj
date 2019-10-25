;; syllables.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given a string (max length 20, containing symbols, spaces, digits, and
;; lowercase letters), count the number of occurrences of vowels (a,e,i,o,u,y)
;; in the string and print that number as X in "The number of syllables is X"
;;
;; input stack has the input string

(ns clojush.problems.software.syllables
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

;; Define test cases
(defn syllables-input
  "Makes a Syllables input of length len."
  [len]
  (apply str
         (repeatedly len
                     #(if (< (lrand) 0.2)
                        (lrand-nth "aeiouy")
                        (lrand-nth (map char (concat (range 32 65) (range 91 127))))))))

; Atom generators
(def syllables-atom-generators
  (concat (list
            "The number of syllables is "
            \a
            \e
            \i
            \o
            \u
            \y
            "aeiouy"
            ;;; end constants
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            (fn [] (syllables-input (lrand-int 21))) ;String ERC
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))

(defn make-syllables-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-syllables-error-function
    ([individual]
      (the-actual-syllables-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-syllables-error-function individual data-cases false))
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
(def syllables-train-and-test-cases
  (map #(sort-by (comp count first) %)
    (train-and-test-cases-from-dataset "syllables" 0 10000000)))

(defn syllables-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first syllables-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second syllables-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn syllables-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Syllables problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-syllables-error-function-from-cases (first syllables-train-and-test-cases)
                                                             (second syllables-train-and-test-cases))
   :atom-generators syllables-atom-generators
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
   :problem-specific-report syllables-report
   :problem-specific-initial-report syllables-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })



;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-max-points 3200)

(reset! global-evalpush-limit 1600)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
(def tom-program
  '(char_empty \y integer_empty "aeiouy" \i char_eq char_allfromstring char_isletter \a \o integer_empty char_isdigit "The number of syllables is " print_string boolean_stackdepth exec_do*count (in1 string_occurrencesofchar boolean_dup_times) boolean_stackdepth print_integer))


;; This program is hand-written
#_(def tom-program
 '(
    in1 integer_inc exec_do*count
    (
      integer_dup integer_mult integer_add
      )
    ))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind false)


                                        ;(test-program-on-testing tom-ind false)

