;; general_digital_multiplier.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; This is code for the digital multiplier problem, as defined in:
;;    Walker, J.A.; Miller, J.F., "The Automatic Acquisition, Evolution and
;;    Reuse of Modules in Cartesian Genetic Programming," Evolutionary
;;    Computation, IEEE Transactions on , vol.12, no.4, pp.397,417, Aug. 2008
;;
;; The n-bit digital multiplier takes 2 n-bit numbers and outputs their
;; product. So, this problem requires 2*n bits of input and 2*n bits of output.
;; The input is stored in a vector as the first item on the auxiliary stack, and
;; the output is stored in a vector as the second item on the auxiliary stack.
;; Each of these vectors has 2*n items, accessed by instructions in0 through
;; in(2*n) and out0 through out(2*n) respectively.
;;
;; Note: This version uses type-tuned genetic source, instead of the limited set
;; used in previous studies and in clojush.problems.boolean.digital-multiplier
;;


(ns clojush.problems.software.general-vector-based-digital-multiplier-3bit
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

(def num-bits-n 3)

(defn int->bits-unmemoized
  [i num-bits]
  (let [conversion (Integer/toString i 2)]
    (concat (repeat (- num-bits (count conversion)) false)
            (map #(= \1 %) conversion))))

(def int->bits (memoize int->bits-unmemoized))

(defn bits->int
  [bits]
  (apply +'
         (map-indexed (fn [index bit]
                        (if bit
                          (expt 2 index)
                          0))
                      (reverse bits))))

; Atom generators
(def atom-generators
  (concat (list
           true
           false
           ;;; end constants
           (fn [] (- (lrand-int 201) 100)) ;Integer ERC [-100,100]
           ;;; end ERCs
           (tag-instruction-erc [:exec :integer :boolean :vector_voolean :vector_integer] 1000)
           (tagged-instruction-erc 1000)
           ;;; end tag ERCs
           'in1
           ;;; end input instructions
           )
          (registered-for-stacks [:exec :integer :boolean :vector_boolean :vector_integer])))

(defn dm-train-cases
  "Generates digital multiplier training cases."
  []
  (for [num1 (range (expt 2 num-bits-n))
        num2 (range (expt 2 num-bits-n))]
    (let [input-bits (vec (concat (int->bits num1 num-bits-n)
                                  (int->bits num2 num-bits-n)))]
      input-bits)))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def data-domains
  [[(dm-train-cases) (expt 2 (* 2 num-bits-n)) 0]
   ])

; Helper function for error function
(defn inputs-outputs-from-inputs
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector in
                 (let [num1 (bits->int (take num-bits-n in))
                       num2 (bits->int (take-last num-bits-n in))]
                   (vec (int->bits (* num1 num2) (* 2 num-bits-n))))))
       inputs))

(defn get-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map inputs-outputs-from-inputs
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def train-and-test-cases
  (get-train-and-test data-domains))

(defn pad-result-to-n-bits
  "Pads vector to have n bits. Uses :no-output as padding."
  [result n]
  (if (>= (count result) n)
    result
    (let [reversed (reverse result)
          padding-length (- n (count result))]
      (reverse (concat reversed (repeat padding-length :no-output))))))

(defn evaluate-program-for-behaviors
  "Evaluates the program on the given list of cases.
   Returns the behaviors, a list of the outputs of the program on the inputs."
  [program cases]
  (flatten
   (doall
    (for [[inputs outputs] cases]
      (let [final-state (run-push program
                                  (->> (make-push-state)
                                       (push-item inputs :input)))
            result (top-item :vector_boolean final-state)]
        (if (= result :no-stack-item)
          (repeat (* 2 num-bits-n) :no-output)
          (pad-result-to-n-bits result (* 2 num-bits-n))))))))

(defn errors-from-behaviors
  "Takes a list of behaviors across the list of cases and finds the error
   for each of those behaviors, returning an error vector."
  [behaviors cases]
  (let [behavior-tuples (partition (* 2 num-bits-n) behaviors)
        output-tuples (map second cases)]
    (flatten
     (map (fn [behavior-vector output-vector]
            (map (fn [behavior output]
                   (cond
                     (= behavior :no-output) 2 ;; Penalty for no output
                     (= behavior output) 0 ;; correct
                     :else 1 ;; incorrect (but has an output)
                     ))
                 behavior-vector
                 output-vector))
          behavior-tuples
          output-tuples))))

(defn error-function
  "The error function. Takes an individual as input,
   and returns that individual with :errors and :behaviors set."
  ([individual]
   (error-function individual :train))
  ([individual data-cases] ;; data-cases should be :train or :test
   (let [cases (case data-cases
                 :train (first train-and-test-cases)
                 :test (second train-and-test-cases)
                 data-cases)
         behaviors (evaluate-program-for-behaviors (:program individual)
                                                   cases)
         errors (errors-from-behaviors behaviors cases)]
     (when (empty? errors)
       (println "empty errors")
       (println errors)
       (println behaviors))
     (if (= data-cases :test)
       (assoc individual :test-errors errors)
       (assoc individual :behaviors behaviors :errors errors)))))

(defn initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn problem-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-with-test (error-function best :test)
        best-test-errors (:test-errors best-with-test)
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Digital Multiplier problem report - generation %s\n" generation)(flush)
    ;; (println "Test total error for best:" best-total-test-error)
    ;; (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    ;; (when (zero? (:total-error best))
    ;;   (doseq [[i error] (map vector
    ;;                          (range)
    ;;                          best-test-errors)]
    ;;     (println (format "Test Case  %3d | Error: %s" i (str error)))))
    ;; (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (doseq [[correct-output program-output]
            (map vector
                 (map second (first train-and-test-cases))
                 (partition (* num-bits-n 2) (:behaviors best)))]
      (println (format "\n| Correct output: %s\n| Program output: %s" (pr-str correct-output) (pr-str program-output))))
    (println ";;******************************")
    ;; return best individual with tests errors added so that those are recorded
    best-with-test))
       ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function error-function
   :training-cases (first train-and-test-cases)
   :sub-training-cases '()
   :atom-generators atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 1600
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1.0}
   :uniform-addition-and-deletion-rate 0.09
   :problem-specific-report problem-report
   :problem-specific-initial-report initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })

