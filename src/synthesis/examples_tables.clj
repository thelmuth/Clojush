(ns synthesis.examples_tables
  (:require [clojure.java.jdbc :as sql]
            [synthesis.db :as db]
            [local-file]))

;;;;;;;;;;
;; Create the examples.

; Query to evolve
; SELECT *
; FROM adult
; WHERE (greater_50k = '>50k')

(def pos-ex
  (vec (db/run-db-function db/synthesis-db
                                    db/db-query
                                    "SELECT *
                                     FROM adult
                                     WHERE (greater_50k = '>50K')")))

(def neg-ex
  (vec (db/run-db-function db/synthesis-db
                                    db/db-query
                                    "SELECT *
                                     FROM adult
                                     WHERE NOT (greater_50k = '>50K')")))

;;;;;;;;;;
;; Create small table for positive and negative examples.

(defn create-and-populate-examples-table
  "Creates table for positive and negative examples, and populates them with pos-ex and neg-ex."
  [pos-ex neg-ex]
  (db/run-db-function db/synthesis-db db/create-table
                      :adult_examples
                      (conj db/synthesis-db-columns [:greater_50k "varchar(8)"]))
  (db/run-db-function db/synthesis-db db/insert-records
                      :adult_examples
                      pos-ex)
  (db/run-db-function db/synthesis-db db/insert-records
                      :adult_examples
                      neg-ex))

(defn drop-examples-table
  "Drops examples table."
  []
  (db/run-db-function db/synthesis-db db/drop-table :adult_examples))

;;;;;;;;;;
;; Example uses

;(create-and-populate-examples-table pos-ex neg-ex)

;(drop-examples-table)

; Displays the example table nicely
#_(sort #(compare (get %1 :occupation) (get %2 :occupation))
      (map #(select-keys % '(:education_num :hours_per_week :occupation))
           neg-ex))

; Displays the count of each occupation
#_(db/run-db-function db/synthesis-db
                           db/db-query
                           "SELECT occupation, count(*)
                            FROM adult
                            GROUP BY occupation"
                           )

#_(count (db/run-db-function db/synthesis-db
                           db/db-query
                           "SELECT *
                            FROM adult
                            WHERE (education_num < 9 AND hours_per_week > 40) OR occupation = 'Farming-fishing'"
                           ))
