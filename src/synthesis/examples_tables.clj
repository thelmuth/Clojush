(ns synthesis.examples_tables
  (:require [clojure.contrib.sql :as sql]
            [synthesis.db :as db]
            [local-file]))

;;;;;;;;;;
;; Create the examples.

(def pos-ex
  (vec (take 50 (db/run-db-function db/synthesis-db
                                    db/db-query
                                    "SELECT *
                                     FROM adult
                                     WHERE age > 40 AND education = 'Masters'"))))

(def neg-ex
  (vec (concat (take 25 (db/run-db-function db/synthesis-db
                                            db/db-query
                                            "SELECT *
                                             FROM adult
                                             WHERE NOT(age > 40)"))
               (take-last 25 (db/run-db-function db/synthesis-db
                                            db/db-query
                                            "SELECT *
                                             FROM adult
                                             WHERE NOT(education = 'Masters')")))))

;;;;;;;;;;
;; Create small tables for positive and negative examples.

(defn create-and-populate-examples-tables
  "Creates tables for positive and negative examples, and populates them with pos-ex and neg-ex."
  [pos-ex neg-ex]
  (db/run-db-function db/synthesis-db db/create-table
                      :adult_pos_ex
                      db/synthesis-db-columns)
  (db/run-db-function db/synthesis-db db/create-table
                      :adult_neg_ex
                      db/synthesis-db-columns)
  (db/run-db-function db/synthesis-db db/insert-records
                      :adult_pos_ex
                      pos-ex)
  (db/run-db-function db/synthesis-db db/insert-records
                      :adult_neg_ex
                      neg-ex))

(defn drop-examples-tables
  "Drops examples tables."
  []
  (db/run-db-function db/synthesis-db db/drop-table :adult_pos_ex)
  (db/run-db-function db/synthesis-db db/drop-table :adult_neg_ex))

