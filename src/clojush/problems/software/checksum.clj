;; checksum.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source:
;;   C. Le Goues et al., "The ManyBugs and IntroClass Benchmarks for Automated Repair of C Programs,"
;;   in IEEE Transactions on Software Engineering, vol. 41, no. 12, pp. 1236-1256, Dec. 1 2015.
;;   doi: 10.1109/TSE.2015.2454513
;;
;; Given a string (max length 50), compute the integer values of the characters
;; in the string, sum them, take the sum modulo 64, add the value of the \space 
;; character, and then convert that integer back into its corresponding character
;; (the checksum). Program must print "Check sum is X", where X is replaced by
;; the correct checksum.
;;
;; input stack has the input string

(ns clojush.problems.software.checksum
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

; Atom generators
(def checksum-atom-generators
  (concat (list
            "Check sum is "
            \space
            64
            ;;; end constants
            (fn [] (- (lrand-int 257) 128)) ;Integer ERC [-128,128]
            (fn [] (lrand-nth (concat [\newline \tab] (map char (range 32 127))))) ;Visible character ERC
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))

(defn make-checksum-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-checksum-error-function
    ([individual]
      (the-actual-checksum-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-checksum-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq
                       [[case-num [input correct-output]] (map-indexed vector (case data-cases
                                                                                :train train-cases
                                                                                :test test-cases
                                                                                []))]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input :input)
                                                      (push-item "" :output)))
                           printed-result (stack-ref :output 0 final-state)]
                       (when print-outputs
                         (println (format "Correct output: %-19s | Program output: %-19s" correct-output printed-result)))
                                        ; Record the behavior

                       ; print if wrong answer
                       (when (not= printed-result correct-output)
                         (println "Wrong result:" input correct-output printed-result))
                       ; print case numbers sometimes
                       (when (= (mod case-num 10000) 0)
                         (println "At case" case-num))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def checksum-train-and-test-cases
  (map #(sort-by (comp count first) %)
       (train-and-test-cases-from-dataset "checksum" 0 100000000)))

(defn checksum-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first checksum-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second checksum-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn checksum-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Checksum problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-checksum-error-function-from-cases (first checksum-train-and-test-cases)
                                                            (second checksum-train-and-test-cases))
   :atom-generators checksum-atom-generators
   :max-points 3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit 1500
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
   :problem-specific-report checksum-report
   :problem-specific-initial-report checksum-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000
   })

;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-evalpush-limit 1500)

(reset! global-max-points 3200)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program works
(def tom-program
 '(
    "Check sum is " print_string
    in1 char_allfromstring
    100 exec_do*times
    (integer_fromchar integer_add)
    64 integer_swap 64 integer_mod
    \space integer_fromchar integer_add
    char_frominteger
    print_char
    ))


(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

;(test-program-on-training tom-ind false)


(test-program-on-testing tom-ind false)
