;; word_stats.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a file with <= 100 characters, print the number of words containing X
;; characters for X from 1 to the length of the longest word, in the format:
;;   words of length 1: 12
;;   words of length 2: 3
;;   ...
;; At the end of the output, should print a line that gives the number of
;; sentences, and line that gives the average number of words per sentence,
;; using the form:
;;  number of sentences: 4
;;  average sentence length: 7.452423455
;; A word is any string of consecutive non-whitespace characters (including
;; sentence terminators). Every input will contain at least one sentence
;; terminator (period, exclamation point, or question mark). The average number
;; of words should simply count the number of words in the file and divide by
;; the number of sentence terminator characters.
;;
;; input stack has the working file on top of stack, full file next

(ns clojush.problems.software.word-stats
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower)
    (:require [clojure.string :as string]))

; Define new instructions
(define-registered
  file_readchar
  ^{:stack-types [:char]}
  (fn [state]
    (if (:autoconstructing state)
      state
      (let [file (top-item :input state)
            first-char (first file)
            inp-result (push-item (apply str (rest file))
                                  :input
                                  (pop-item :input state))]
        (if (= file "")
          state
          (push-item first-char
                     :char
                     inp-result))))))

(define-registered
  file_readline
  ^{:stack-types [:string]}
  (fn [state]
    (if (:autoconstructing state)
      state
      (let [file (top-item :input state)
            index (inc (.indexOf file "\n"))
            has-no-newline (= 0 index)
            inp-result (push-item (if has-no-newline
                                    ""
                                    (subs file index))
                                  :input
                                  (pop-item :input state))]
        (if (= file "")
          state
          (if has-no-newline
            (push-item file :string inp-result)
            (push-item (subs file 0 index)
                       :string
                       inp-result)))))))

(define-registered
  file_EOF
  ^{:stack-types [:boolean]}
  (fn [state]
    (if (:autoconstructing state)
      state
      (let [file (top-item :input state)
            result (empty? file)]
        (push-item result :boolean state)))))

(define-registered
  file_begin
  ^{:stack-types [:input]}
  (fn [state]
    (if (:autoconstructing state)
      state
      (push-item (stack-ref :input 1 state)
                 :input
                 (pop-item :input state)))))

; Atom generators
(def word-stats-atom-generators
  (concat (list
            \. \? \! \space \tab \newline
            []
            "words of length "
            ": "
            "number of sentences: "
            "average sentence length: "
            ;;; end constants
            (fn [] (- (lrand-int 201) 100)) ;Integer ERC
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'file_readline
            'file_readchar
            'file_EOF
            'file_begin
            ;;; end input instructions
            )
          (registered-for-stacks [:string :vector_string :char :integer :vector_integer :float :vector_float :boolean :exec :print])))

(defn make-word-stats-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-word-stats-error-function
    ([individual]
      (the-actual-word-stats-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-word-stats-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doseq
                       [[case-num [input1 correct-output]] (map-indexed vector (case data-cases
                                                                                :train train-cases
                                                                                :test test-cases
                                                                                []))]
                     (let [sentences (count (filter #(some #{%} ".?!") input1))
                           words (filter not-empty (string/split input1 #"\s+"))
                           words-per-sentence (float (/ (count words) sentences))
                           final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)
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
(def word-stats-train-and-test-cases
  (map #(sort-by (comp count first) %)
    (train-and-test-cases-from-dataset "word-stats" 0 1000000000)))

(defn word-stats-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first word-stats-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second word-stats-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn word-stats-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Word Stats problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (<= (:total-error best) 0.02)
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
  {:error-function (make-word-stats-error-function-from-cases (first word-stats-train-and-test-cases)
                                                              (second word-stats-train-and-test-cases))
   :atom-generators word-stats-atom-generators
   :max-points 3200
   :max-genome-size-in-initial-program 400
   :evalpush-limit 6000
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
   :problem-specific-report word-stats-report
   :problem-specific-initial-report word-stats-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :error-threshold 0.02
   :max-error 10000
   })



;;;;;;;
;; Below here is for testing push programs against stored data

(reset! global-max-points 3200)

(reset! global-evalpush-limit 6000)

(defn test-program-on-training
 [program print-outputs]
 ((:error-function argmap) program :train print-outputs))

(defn test-program-on-testing
 [program print-outputs]
 ((:error-function argmap) program :test print-outputs))

;;This program is an evolved solution
#_(def tom-program
  '(([]) (in1 (vector_integer_pushall) (exec_do*vector_integer (integer_add vector_integer_conj))))
  )


;; This program is hand-written
(def tom-program
 '(
    []
    exec_do*while ;for each line in the file
    (
      file_readline string_split
      exec_do*while ;for each string in this line
      (
        string_length
        integer_dup
        vector_integer_dup vector_integer_length
        integer_gte
        exec_while ;make sure vector_integer is long enough
        (
          0 vector_integer_conj
          integer_dup
          vector_integer_dup vector_integer_length
          integer_gte
          )
        ;Inc that integer's index in vector
        integer_dup vector_integer_dup
        vector_integer_nth
        integer_inc integer_swap
        vector_integer_set
        string_empty boolean_not
        )
      file_EOF boolean_not
      )
    ; Have vector of word size counts at this point
    1 vector_integer_dup vector_integer_length integer_dec
    exec_do*range
    (
      "words of length " print_string
      integer_dup print_integer
      ": " print_string
      vector_integer_dup vector_integer_nth print_integer
      print_newline
      )
    "number of sentences: " print_string
    file_begin
    exec_do*while
    ( ;get all lines in one string
      file_readline string_concat
      file_EOF boolean_not
      )
    ; count \., \!, and \?
    string_dup string_dup string_dup
    \. string_occurrencesofchar
    \! string_occurrencesofchar
    \? string_occurrencesofchar
    integer_add integer_add integer_dup
    print_integer print_newline
    "average sentence length: " print_string
    string_split string_stackdepth
    float_frominteger float_frominteger float_div print_float
    ))







(def tom-ind
  {:program tom-program})


;;; This is how you run the program once.
#_(run-push tom-program
          (push-item "oldowestact" :input (push-item "clinteastwood" :input (make-push-state))))

;;; This makes sure the program works on all test and train cases:

(test-program-on-training tom-ind false)


(test-program-on-testing tom-ind false)



