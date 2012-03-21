(ns synthesis.metrics
  (:require [synthesis.examples_tables :as et]
            [synthesis.db :as db]
            [synthesis.qfe :as qfe]))


;; Setup examples table
(et/drop-examples-table)
(et/create-and-populate-examples-table et/pos-ex et/neg-ex)

;; Desired query Q0 WHERE clause
(def Q0-where
  "WHERE (age < 28 OR age > 48) AND (hours_per_week < 35 OR hours_per_week > 46)")

;; Evolved solution query Q WHERE clause
(def Q-where
  "WHERE (hours_per_week <> 40 AND (((age < 28 AND race > 'Black') AND workclass <= 'Self-emp-inc') OR age >= 49))")


;; Precision, recall, and f1-score of Q-where on examples

(let [positive-examples et/pos-ex
      negative-examples et/neg-ex
      result-query-string (str "SELECT *
                                FROM adult_examples\n"
                               Q-where)
      query-future (future
                     (db/run-db-function db/synthesis-db
                                         db/db-query
                                         result-query-string))]
  (try
    (let [result-rows (.get query-future 500
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
      (println "\n\nTrue positives: " true-positives)
      (println "False positives: " false-positives)
      (println "True negatives: " true-negatives)
      (println "False negatives: " false-negatives)
      (println "Total Positives: " (count positive-examples))
      (println "Total Negatives: " (count negative-examples))
      (println "Total: " (+ (count negative-examples) (count positive-examples)) "\n")
      (println "Precision: " (qfe/precision true-positives false-positives))
      (println "Recall: " (qfe/recall true-positives false-negatives))
      (println "F1-Score: " (qfe/f1-score true-positives false-positives false-negatives) "\n")
      error)
    (catch java.util.concurrent.TimeoutException e
           (when (not (future-cancel query-future))
             (println "future could not be cancelled"))
           2)))

