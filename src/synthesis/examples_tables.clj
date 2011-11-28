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
;; Create small tables for positive and negative examples

