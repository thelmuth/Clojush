;; pendulum_sort.clj
;; Peter Kelly, pxkelly@hamilton.edu
;;
;;

(ns clojush.problems.software.pendulum-sort
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower combinatorics]
        ))

;; Define test cases
(defn pendulum-sort-input
  "Makes a Pendulum Sort input vector of length len."
  [len]
  (vec (repeatedly len #(- 10000 (rand-int 20000)))))

; Atom generators
(def pendulum-sort-atom-generators
  (concat (list
            []
            ;;; end constants
            (fn [] (pendulum-sort-input (inc (lrand-int 50)))) ;Vector ERC
            ;;; end ERCs
            (tag-instruction-erc [:vector_integer :integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:vector_integer :integer :boolean :exec])))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def pendulum-sort-data-domains
  [[(list [0] [100] [-100] [1000] [-1000]) 5 0] ;; Length-1 vectors
   [(fn [] (pendulum-sort-input 1)) 45 500] ;; Random Length-1 vectors
   [(list [2 129]
          [1 -4]
          [999 74]
          [987 995]
          [-788 -812]) 5 0] ;; Length-2 vectors
   [(fn [] (pendulum-sort-input 2)) 45 500] ;; Random Length-2 vectors
   [(fn [] (pendulum-sort-input (+ 3 (lrand-int 3)))) 50 500] ;; Random Length-3, -4, and -5 vectors
   [(fn [] (pendulum-sort-input 50)) 5 100] ;; Random Length-50 vectors
   [(fn [] (pendulum-sort-input (inc (lrand-int 50)))) 95 1000] ;; Random length, random ints
   ])

;;Can make Pendulum Sort test data like this:
;(test-and-train-data-from-domains pendulum-sort-data-domains)

; Helper function for error function
(defn pendulum-sort-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
        (vector in
          (loop [pendulum [] remain in left true]
            (cond
              (= (count remain) 0) (vec pendulum)
              (= left true) (recur (flatten (conj (list pendulum) (apply min remain))) (rest (sort remain)) false)
              :else (recur (flatten (conj (vec pendulum) (apply min remain))) (rest (sort remain)) true)))))
       inputs))

(defn make-pendulum-sort-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-pendulum-sort-error-function
    ([individual]
     (the-actual-pendulum-sort-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-pendulum-sort-error-function individual data-cases false))
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
                           result (top-item :vector_integer final-state)]
                       (when print-outputs
                           (println (format "Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                       ; Record the behavior
                       (swap! behavior conj result)
                       ; Error is integer error at each position in the vectors, with additional penalties for incorrect size vector
                       (if (vector? result)
                         (+' (apply +' (map (fn [cor res]
                                              (abs (- cor res)))
                                            correct-output
                                            result))
                             (*' 10000 (abs (- (count correct-output) (count result))))) ; penalty of 10000 times difference in sizes of vectors
                         1000000000) ; penalty for no return value
                       )))]
       (if (= data-cases :train)
         (assoc individual :behaviors @behavior :errors errors)
         (assoc individual :test-errors errors))))))

(defn get-pendulum-sort-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map pendulum-sort-test-cases
       (test-and-train-data-from-domains data-domains)))

; Define train and test cases
(def pendulum-sort-train-and-test-cases
  (get-pendulum-sort-train-and-test pendulum-sort-data-domains))

(defn pendulum-sort-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first pendulum-sort-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second pendulum-sort-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn pendulum-sort-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Pendulum Sort problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-pendulum-sort-error-function-from-cases (first pendulum-sort-train-and-test-cases)
                                                                  (second pendulum-sort-train-and-test-cases))
   :atom-generators pendulum-sort-atom-generators
   :max-points 2000
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
   :problem-specific-report pendulum-sort-report
   :problem-specific-initial-report pendulum-sort-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 1000000000
   })
