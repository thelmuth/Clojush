;; backspace.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.backspace
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

; Atom generators
(def backspace-atom-generators
  (concat (list
            ;;; end ERCs
            (tag-instruction-erc [:string :char :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:string :char :integer :boolean :exec])))

(defn choose-char
  [type]
  (if (= type 3)  ; 1/4 chance for #
    \#
    (char (+ (lrand-int 90) 36))))

;; Define test cases
(defn backspace-input
  "Makes a Backspace input of length len"
  [len]
  (apply str (repeatedly len #(choose-char (lrand-int 4)))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def backspace-data-domains
  [[(list ""
          " "
          "a"
          "abc#d##c"
          "abc#d###"
          "#################################"
          "This is a semi-long string without any backspace characters"
          "T#h#i#s# #is a #s#e#m#i#-#l#o#n#g# string #w#i#t#h# lots o#f# #backspace #c#h#a#r#a#c#t#e#r#s#"
          ) 8 0]
   [(fn [] (backspace-input (lrand-int 1000))) 192 2000] ;; Random length
   ])

;;Can make Backspace test data like this:
;(test-and-train-data-from-domains backspace-data-domains)

(defn backspace
  [string index]
  (if (= index 0)
    (apply str (rest string))
    (str (subs string 0 (dec index)) (subs string (inc index)))))

; Helper function for error function
(defn backspace-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
          (vector in
            (loop [current in]
              (if (clojure.string/includes? current "#")
                (recur (backspace current (.indexOf current "#")))
                current))))
       inputs))

(defn make-backspace-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-backspace-error-function
    ([individual]
     (the-actual-backspace-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-backspace-error-function individual data-cases false))
    ([individual data-cases print-outputs]
     (let [behavior (atom '())
           errors (doall
                   (for [[input1 correct-output] (case data-cases
                                                   :train train-cases
                                                   :test test-cases
                                                   [])]
                     (let [final-state (run-push (:program individual)
                                                 (->> (make-push-state)
                                                      (push-item input1 :input)))
                           result (top-item :string final-state)]
                       (when print-outputs
                           (println (format "Correct output: %s\n| Program output: %s\n" correct-output (str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is levenshtein distance
                         (levenshtein-distance correct-output (str result))
                         )))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-backspace-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map backspace-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def backspace-train-and-test-cases
  (get-backspace-train-and-test backspace-data-domains))

(defn backspace-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first backspace-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second backspace-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn backspace-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Backspace problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-backspace-error-function-from-cases (first backspace-train-and-test-cases)
                                                                  (second backspace-train-and-test-cases))
   :atom-generators backspace-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 2000
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
   :problem-specific-report backspace-report
   :problem-specific-initial-report backspace-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
