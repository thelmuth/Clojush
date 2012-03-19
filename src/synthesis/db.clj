(ns synthesis.db
  (:require [clojure.contrib.sql :as sql]
            [local-file]))

;;;;;;;;;;
;; Generic db functions

(defn run-db-function
  "Function to run db-function on optional args in the database."
  [database db-function & args]
  (sql/with-connection
    database
    (sql/transaction
      (apply db-function args))))

(defn create-table
  "Creates the table."
  [table columns-vector]
  (apply sql/create-table
         (cons table columns-vector)))

(defn drop-table
  "Drop a table"
  [table]
  (try
    (sql/drop-table table)
    (catch Exception _)))

(defn insert-rows
  "Inserts one or more rows into table."
  [table & rows]
  (apply sql/insert-rows
         (cons table rows)))

(defn insert-records
  "Inserts one or more records into table. Rows must be a list of the records."
  [table rows]
  (apply sql/insert-records
         (cons table rows)))

(defn read-all
  "Read an entire table and return as a vector of maps."
  [table]
  (sql/with-query-results res
                          [(str "SELECT * FROM " (name table))]
                          (into [] res)))

(defn db-query
  "Retruns results of query as a vector of maps. query is a string representing an SQL query"
  [query]
  (sql/with-query-results res
                          [query]
                          (into [] res)))


;;;;;;;;;;
;; Small database example
;
;; Database attributes
;#_(def synthesis-db {:classname "org.sqlite.JDBC"
;                   :subprotocol "sqlite"
;                   :subname (local-file/file* "db/synthesis.sqlite3")
;                   :create true})
;
;;Drop table people
;#_(run-db-function synthesis-db drop-table :people)
;
;;Create table people
;#_(run-db-function synthesis-db create-table
;                 :people
;                 [[:pid :int "PRIMARY KEY"]
;                  [:firstname "varchar(32)"]
;                  [:lastname "varchar(32)"]])
;
;;Insert rows into people
;#_(run-db-function synthesis-db insert-rows
;                 :people
;                 [11 "Tom" "Smith"]
;                 [19 "William" "Williams"]
;                 [203 "Gene" "Wirth"]
;                 [54 "Penelope" "Davies"]
;                 [32 "Zach" "Francis"]
;                 [179 "Allen" "Osborn"])
;
;;Read the whole people table
;#_(run-db-function synthesis-db read-all :people)
;
;;Query people table
;#_(run-db-function synthesis-db db-query "SELECT pid, firstname
;                                        FROM people
;                                        WHERE pid<100")
;
;;Find people whose last names are later in the alphabet than their first names
;#_(run-db-function synthesis-db db-query "SELECT firstname, lastname
;                                        FROM people
;                                        WHERE firstname<lastname")
;
;
;
;;List pairs of lastnames where the first last name is earlier in the alphabet than the second
;#_(run-db-function synthesis-db db-query "SELECT P1.lastname as Earlier, P2.lastname as Later
;                                        FROM people P1, people P2
;                                        WHERE P1.lastname < P2.lastname")
                 

;;;;;;;;;;
;; Adult database

; Database attributes
(def synthesis-db {:classname "org.sqlite.JDBC"
                   :subprotocol "sqlite"
                   :subname (local-file/file* "db/synthesis.sqlite3")
                   :create true})

(def synthesis-db-columns
  [[:age :int]
   [:workclass "varchar(32)"]
   [:fnlwgt :int]
   [:education "varchar(32)"]
   [:education_num :int]
   [:marital_status "varchar(32)"]
   [:occupation "varchar(32)"]
   [:relationship "varchar(32)"]
   [:race "varchar(32)"]
   [:sex "varchar(8)"]
   [:capital_gain :int]
   [:capital_loss :int]
   [:hours_per_week :int]
   [:native_country "varchar(32)"]
   [:greater_50k "varchar(8)"]])

(def synthesis-db-columns-map
  (apply hash-map (flatten synthesis-db-columns)))

;;;;;;;;;;
;; Example usages

;Drop table adult
#_(run-db-function synthesis-db drop-table :adult)

;Create table adult
#_(run-db-function synthesis-db create-table
                 :adult
                 synthesis-db-columns)


; List people who are older than 55 and marital status is Never-married and education is Masters
#_(run-db-function synthesis-db
                 db-query
                 "SELECT *
                  FROM adult
                  WHERE age > 55 AND marital_status = 'Never-married' AND education = 'Masters'")

; Select a random row
#_(run-db-function synthesis-db db-query "SELECT *
                                        FROM adult
                                        ORDER BY RANDOM()
                                        LIMIT 1")

; Get the number of distinct values for each attribute of the database.
#_(map #(first (let [attribute (name %)
            query (str "SELECT count(DISTINCT "
                       attribute
                       ")
                        FROM adult")]
        (run-db-function synthesis-db db-query query)))
     (map first synthesis-db-columns))
