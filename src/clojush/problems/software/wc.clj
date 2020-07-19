;; wc.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; This is code for the problem emulating the "wc" unix command,
;; which returns the character, word, and line counts of a string
;; Outputs are integers that come from one
;; of the three output instructions.
;;

;; NOTE: A word is defined by any characters separated by any number
;; of spaces, tabs, and newlines. Any other character can be part of
;; a word.

(ns clojush.problems.software.wc
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        [clojure.string :only [split trim]]))

; Define new instructions
; (define-registered
;   string_readchar
;   (fn [state]
;     (let [file (top-item :auxiliary state)
;           first-char (first file)
;           aux-result (push-item (apply str (rest file))
;                                 :auxiliary
;                                 (pop-item :auxiliary state))]
;       (if (= file "")
;         state
;         (push-item (str first-char)
;                    :string
;                    aux-result)))))

; (define-registered
;   string_readline
;   (fn [state]
;     (let [file (top-item :auxiliary state)
;           index (inc (.indexOf file "\n"))
;           has-no-newline (= 0 index)
;           aux-result (push-item (if has-no-newline
;                                   ""
;                                   (subs file index))
;                                 :auxiliary
;                                 (pop-item :auxiliary state))]
;       (if (= file "")
;         state
;         (if has-no-newline
;           (push-item file :string aux-result)
;           (push-item (subs file 0 index)
;                      :string
;                      aux-result))))))

; (define-registered 
;   string_whitespace ;;returns true if top string is entirely composed of spaces, tabs, and newlines (even if empty)
;   (fn [state]
;     (if (not (empty? (:string state)))
;       (push-item (every? #{\space \tab \newline} (top-item :string state))
;                  :boolean
;                  (pop-item :string state))
;       state)))

; (define-registered
;   file_EOF
;   (fn [state]
;     (let [file (top-item :auxiliary state)
;           result (empty? file)]
;       (push-item result :boolean state))))

; (define-registered
;   file_begin
;   (fn [state]
;     (push-item (stack-ref :auxiliary 1 state)
;                :auxiliary
;                (pop-item :auxiliary state))))

(define-registered
  output_charcount
  ^{:stack-types [:integer]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :output 0
                     (pop-item :integer state))))))

(define-registered
  output_wordcount
  ^{:stack-types [:integer]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :output 1
                     (pop-item :integer state))))))

(define-registered
  output_linecount
  ^{:stack-types [:integer]}
  (fn [state]
    (if (empty? (:integer state))
      state
      (let [top-int (top-item :integer state)]
        (stack-assoc top-int :output 2
                     (pop-item :integer state))))))

; Atom generators
(def wc-atom-generators
  (concat
   (list
    \space
    \tab
    \newline
    (fn [] (- (lrand-int 201) 100))
    (fn [] (lrand-nth ["\n" "\t" " "]))
    (fn [] (lrand-nth (concat ["\n" "\t"] (map (comp str char) (range 32 127)))))
    (tag-instruction-erc [:exec :string :integer] 1000)
    (tagged-instruction-erc 1000)
    ;;;; end ERCs
    'in1
    ;;;; end input instructions
    ; 'string_readchar
    ; 'string_readline
    ; 'string_whitespace
    ; 'file_EOF
    ; 'file_begin
    ; 'output_charcount
    ; 'output_wordcount
    ; 'output_linecount
    ;;;; end problem-specific instructions
    ; 'string_pop
    ; 'string_take
    ; 'string_eq
    ; 'string_stackdepth
    ; 'string_rot
    ; 'string_parse_to_chars
    ; ;'string_rand
    ; 'string_contains
    ; 'string_reverse
    ; 'string_yank
    ; 'string_swap
    ; 'string_yankdup
    ; 'string_flush
    ; 'string_length
    ; 'string_concat
    ; ;'string_atoi
    ; 'string_shove
    ; 'string_dup
    ; 'string_split
    ; ;;; end string instructions
    ; 'integer_add
    ; 'integer_swap
    ; 'integer_yank
    ; 'integer_dup
    ; 'integer_yankdup
    ; 'integer_shove
    ; 'integer_mult
    ; 'integer_div
    ; 'integer_max
    ; 'integer_sub
    ; 'integer_mod
    ; 'integer_rot
    ; 'integer_min
    ; 'integer_inc
    ; 'integer_dec
    ; ;;; end integer instructions
    ; 'exec_y
    ; 'exec_pop
    ; 'exec_eq
    ; 'exec_stackdepth
    ; 'exec_rot
    ; 'exec_when
    ; 'exec_do*times
    ; 'exec_do*count
    ; 'exec_s
    ; 'exec_do*range
    ; 'exec_if
    ; 'exec_k
    ; 'exec_yank
    ; 'exec_yankdup
    ; 'exec_swap
    ; 'exec_dup
    ; 'exec_shove
    ; ;;; end exec instructions
    ; 'boolean_swap
    ; 'boolean_and
    ; 'boolean_not
    ; 'boolean_or
    ; 'boolean_frominteger
    ; 'boolean_stackdepth
    ; 'boolean_dup
    
    )
(registered-for-stacks [:string :char :integer :boolean :exec])
))

;; Define test cases
(defn wc-input
  "Makes a WC input of length len."
  [len]
  (apply str
         (repeatedly len
                     (fn []
                       (let [rand-cond (lrand)]
                         (cond
                           (< rand-cond 0.05) \tab
                           (< rand-cond 0.1) \newline
                           (< rand-cond 0.35) \space
                           :else (lrand-nth (map char (range 32 127)))))))))

;; A list of data domains for the WC problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def wc-data-domains
  [[(list "", "A", " ", "\t", "\n", "5", "#", "A\n", " \n", "\t\n", "\n\n",
          (apply str (repeat 100 \newline))
          (apply str (repeat 100 \space))
          (apply str (repeat 100 \tab))
          (apply str (repeat 100 \A))
          (apply str (take 100 (cycle (list \A \newline))))
          (apply str (take 100 (cycle (list \B \newline \newline))))
          (apply str (take 100 (cycle (list \C \D \newline))))
          (apply str (take 100 (cycle (list \E \space))))
          (apply str (take 100 (cycle (list \F \tab))))
          (apply str (take 100 (cycle (list \x \newline \y \space))))
          (apply str (take 100 (cycle (list \space \newline))))) 22 0] ;; "Special" inputs covering most base cases
   [(fn [] (str (wc-input (inc (lrand-int 99))) \newline)) 20 500] ;; Inputs ending in a newline
   [(fn [] (wc-input (inc (lrand-int 100)))) 158 1500] ;; Inputs that may or may not end in a newline
   ])

;;Can make WC test data like this:
;(test-and-train-data-from-domains wc-data-domains)

; Helper functions for error function
(defn wc-char-count
  "Takes a wc input and returns the char count output"
  [input]
  (count input))

(defn wc-word-count
  "Takes a wc input and returns the word count output"
  [input]
  (count (filter not-empty (split (trim input) #"\s+"))))
    
(defn wc-line-count
  "Takes a wc input and returns the line count output. Only lines ending
   newlines count toward total."
  [input]
  (get (frequencies input) \newline 0))

(defn wc-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output-char output-word output-line]."
  [inputs]
  (map #(vector %
                (wc-char-count %)
                (wc-word-count %)
                (wc-line-count %))
       inputs))

;; Define error function. For now, each run uses different random inputs
(defn make-wc-error-function-from-cases
  "Returns the error function for the wc problem. Takes as
   input train and test cases."
  [train-cases test-cases]
  (fn the-actual-wc-error-function
    ([individual]
     (the-actual-wc-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-wc-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (flatten
                   (doall
                    (for [[input out-char out-word out-line] (case data-cases
                                                               :train train-cases
                                                               :test test-cases
                                                               data-cases)]
                      (let [final-state (run-push (:program individual)
                                                  (->> (make-push-state)
                                                       (push-item :no-output :output)
                                                       (push-item :no-output :output)
                                                       (push-item :no-output :output)
                                                       (push-item input :input)))
                            result-char (stack-ref :output 0 final-state)
                            result-word (stack-ref :output 1 final-state)
                            result-line (stack-ref :output 2 final-state)]
                        (when print-outputs
                          (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str [out-char out-word out-line])
                                           (pr-str [result-char result-word result-line]))))
                        ; Record the behavior
                        (swap! behavior concat [result-char result-word result-line])
                        ; The error is the integer difference between the desired output
                        ; and the result output for each of char-count, word-count, and
                        ; line-count. Because of this, each test case results in 3
                        ; error values. If a value is missing, a penalty of 100000
                        ; is given.
                        (vector (if (number? result-char)
                                  (abs (- result-char out-char))
                                  100000)
                                (if (number? result-word)
                                  (abs (- result-word out-word))
                                  100000)
                                (if (number? result-line)
                                  (abs (- result-line out-line))
                                  100000))))))]
        (if (= data-cases :test)
          (assoc individual :test-errors errors)
          (assoc individual :behaviors @behavior :errors errors))))))

(defn get-wc-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map wc-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def wc-train-and-test-cases
  (get-wc-train-and-test wc-data-domains))

(defn wc-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first wc-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second wc-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn wc-report
  "Customize generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- WC problem report generation %s\n" generation) (flush)
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
    (println ";;******************************"))) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.

; Define the argmap
(def argmap
  {:error-function (make-wc-error-function-from-cases (first wc-train-and-test-cases)
                                                      (second wc-train-and-test-cases))
   :atom-generators wc-atom-generators
   :training-cases (first wc-train-and-test-cases)
   :sub-training-cases '()
   :max-points 4000
   :max-genome-size-in-initial-program 400
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5}
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report wc-report
   :problem-specific-initial-report wc-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 100000
   })
