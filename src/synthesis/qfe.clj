(ns synthesis.qfe
  (:require [clojush]
            [synthesis.core :as synth-core]
            [synthesis.db :as db]))

;;;;;;;;;;
;; Helper functions

(defn precision
  [true-positives false-positives]
  (float (/ true-positives (+ true-positives false-positives))))

(defn recall
  [true-positives false-negatives]
  (float (/ true-positives (+ true-positives false-negatives))))

(defn f1-score
  ([true-positives false-positives false-negatives]
    (f1-score (precision true-positives false-positives)
              (recall true-positives false-negatives)))
  ([prec rec]
    (/ (* 2 prec rec) (+ prec rec))))

;;;;;;;;;;
;; Error function

(defn qfe-error-function-creator
  "Creates an error function based on vectors positive-examples and negative-examples."
  [positive-examples negative-examples]
  (fn [program]
    (list
      (let [final-state (clojush/run-push
                          program
                          (clojush/push-item "adult"
                                             :from
                                             (clojush/push-item "*"
                                                                :select
                                                                (clojush/make-push-state))))
            result-query-string (synth-core/stacks-to-query-string final-state)
            query-future (future
                           (db/run-db-function db/synthesis-db
                                               db/db-query
                                               result-query-string))]
        (println "---")
        (println "Query:")
        (println result-query-string)
        (try
          (let [result-rows (.get query-future 1000 (java.util.concurrent.TimeUnit/MILLISECONDS))
                true-positives (count (clojure.set/intersection (set positive-examples)
                                                                (set result-rows)))
                false-positives (count (clojure.set/intersection (set negative-examples)
                                                                 (set result-rows)))]
            (println "True positives:" true-positives)
            (println "False positives:" false-positives)
            (println "Error:" (+ false-positives (- (count positive-examples) true-positives)))
            (+ false-positives (- (count positive-examples) true-positives)))
          (catch java.util.concurrent.TimeoutException e
                 (if (future-cancel query-future)
                   (println "future cancelled")
                   (println "future could not be cancelled"))
                 100000)))))) ;;penalty of 100000 for not returning

;;;;;;;;;;
;; Main pushgp calling function

(defn query-from-examples
  "Takes vectors of positive and negative row examples and starts a pushgp run to find a query that
   matches those examples."
  [positive-examples negative-examples]
  (clojush/pushgp
    :error-function (qfe-error-function-creator positive-examples negative-examples)
    :atom-generators synth-core/qfe-atom-generators
    :max-points 250
    :evalpush-limit 300
    :population-size 100
    :max-generations 40
    :tournament-size 7
    :report-simplifications 0
    :final-report-simplifications 10
    :reproduction-simplifications 1
    :use-single-thread true))

;;;;;;;;;;
;; Create the examples.

#_(vec (take 10 (db/run-db-function db/synthesis-db
                                    db/db-query
                                    "SELECT *
                                     FROM adult
                                     WHERE age > 55 AND education = 'Masters'")))

(def pos-ex
  (vec (take 30 (db/run-db-function db/synthesis-db
                                    db/db-query
                                    "SELECT *
                                     FROM adult
                                     WHERE age > 55 AND education = 'Masters'"))))

#_(def neg-ex
  (vec (take 30 (db/run-db-function db/synthesis-db
                                    db/db-query
                                    "SELECT *
                                     FROM adult
                                     WHERE NOT(age > 55 AND education = 'Masters')"))))

(def neg-ex
  (vec (concat (take 15 (db/run-db-function db/synthesis-db
                                            db/db-query
                                            "SELECT *
                                             FROM adult
                                             WHERE NOT(age > 55)"))
               (take-last 15 (db/run-db-function db/synthesis-db
                                            db/db-query
                                            "SELECT *
                                             FROM adult
                                             WHERE NOT(education = 'Masters')")))))

;;;;;;;;;;
;; Example usage

(query-from-examples pos-ex neg-ex)
