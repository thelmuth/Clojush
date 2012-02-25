(ns synthesis.examples_tables
  (:require [clojure.contrib.sql :as sql]
            [synthesis.db :as db]
            [local-file]))

;;;;;;;;;;
;; Create the examples.

; Query to evolve
; SELECT count(*)
; FROM adult
; WHERE (education_num < 8 AND hours_per_week > 40) OR (education_num > 12 AND hours_per_week < 40)

(def pos-ex
  (vec (take 50 (db/run-db-function db/synthesis-db
                                    db/db-query
                                    "SELECT *
                                     FROM adult
                                     WHERE (education_num < 8 AND hours_per_week > 40) OR (education_num > 12 AND hours_per_week < 40)"))))

(def neg-ex
  (vec (take 50 (db/run-db-function db/synthesis-db
                                            db/db-query
                                            "SELECT *
                                             FROM adult
                                             WHERE NOT ((education_num < 8 AND hours_per_week > 40) OR (education_num > 12 AND hours_per_week < 40))"))))

;;;;;;;;;;
;; Create small table for positive and negative examples.

(defn create-and-populate-examples-table
  "Creates table for positive and negative examples, and populates them with pos-ex and neg-ex."
  [pos-ex neg-ex]
  (db/run-db-function db/synthesis-db db/create-table
                      :adult_examples
                      db/synthesis-db-columns)
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
#_(sort #(< (get %1 :hours_per_week) (get %2 :hours_per_week))
      (map #(select-keys % '(:education_num :hours_per_week))
           pos-ex))
