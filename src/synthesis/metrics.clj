(ns synthesis.metrics
  (:require [synthesis.examples_tables :as et]
            [synthesis.db :as db]
            [synthesis.qfe :as qfe]))


;; Setup examples table
(et/drop-examples-table)
(et/create-and-populate-examples-table et/pos-ex et/neg-ex)

;; Desired query Q0 WHERE clause
(def Q0-where
  "(greater_50k = '>50K')")

;; Evolved solution query Q WHERE clause
(def Q-where
  "((((education_num >= 10 AND marital_status = 'Married-civ-spouse') OR education_num >= 15) AND age >= 28) OR capital_gain > 4787)")


;; Precision, recall, and f1-score of Q-where 

(defn print-metrics
  [database-string where-clause positive-examples negative-examples]
  (let [result-query-string (str "SELECT *\nFROM "
                                 database-string
                                 "\nWHERE "
                                 where-clause)
        query-future (future
                       (db/run-db-function db/synthesis-db
                                           db/db-query
                                           result-query-string))]
    (println "\n\nQuery:")
    (println result-query-string)
    (try
      (let [result-rows (.get query-future 2000
                              (java.util.concurrent.TimeUnit/MILLISECONDS))
            true-positives (count (clojure.set/intersection (set positive-examples)
                                                            (set result-rows)))
            false-positives (count (clojure.set/intersection (set negative-examples)
                                                             (set result-rows)))
            true-negatives (- (count negative-examples) false-positives)
            false-negatives (- (count positive-examples) true-positives)
            error (- 1.0 (qfe/f1-score true-positives
                                       false-positives
                                       (- (count positive-examples) true-positives)))]
        (println "\nTotal Actual Positives: " (count positive-examples))
        (println "Total Actual Negatives: " (count negative-examples))
        (println "Total: " (+ (count negative-examples) (count positive-examples)))
        (println "\nTrue positives: " true-positives)
        (println "True negatives: " true-negatives)
        (println "False negatives: " false-negatives)
        (println "False positives: " false-positives)
        (println "\nAccuracy: " (float (/ (+ true-positives true-negatives)
                                          (+ (count negative-examples) (count positive-examples)))))
        (println "Precision: " (qfe/precision true-positives false-positives))
        (println "Recall: " (qfe/recall true-positives false-negatives))
        (println "F1-Score: " (qfe/f1-score true-positives false-positives false-negatives) "\n")
        nil)
      (catch java.util.concurrent.TimeoutException e
             (when (not (future-cancel query-future))
               (println "future could not be cancelled"))
             nil))))

;; Print metrics over examples
(print-metrics "adult_examples" Q-where et/pos-ex et/neg-ex)

;; Print metrics over entire table
(print-metrics "adult"
               Q-where
               (db/run-db-function db/synthesis-db
                                   db/db-query
                                   (str "SELECT *
                                         FROM adult
                                         WHERE "
                                        Q0-where))
               (db/run-db-function db/synthesis-db
                                   db/db-query
                                   (str "SELECT *
                                         FROM adult
                                         WHERE NOT("
                                        Q0-where
                                        ")")))

;; Print metrics over test data table
(print-metrics "adult_test"
               Q-where
               (db/run-db-function db/synthesis-db
                                   db/db-query
                                   (str "SELECT *
                                         FROM adult_test
                                         WHERE "
                                        Q0-where))
               (db/run-db-function db/synthesis-db
                                   db/db-query
                                   (str "SELECT *
                                         FROM adult_test
                                         WHERE NOT("
                                        Q0-where
                                        ")")))
