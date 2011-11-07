;; synthesis.core.clj
;; The main code for query creation.
;; Tom Hemuth, thelmuth@cs.umass.edu, 2011

(ns synthesis.core
  (:require [clojure.contrib.sql :as sql]
            [clojush]
            [synthesis.db_creation :as db]))

;;;;;;;;;;
;; Query creation.
;;
;; SFW maps are of the following form:
;; :select ("column1" "column2" "column3" ... "columnN")
;; :from ("table1" "table2" ... "tableM")
;; :where '("clause1" AND/OR "clause2" AND/OR ... "clauseZ")

(defn sfw-map-to-query-string
  "Takes a map of a SFW query and returns a string representation of that query."
  [swf-map]
  (str "SELECT " (apply str (interpose ", " (get swf-map :select))) \newline
       "FROM " (apply str (interpose ", " (get swf-map :from))) \newline
       "WHERE " (apply str (interpose " " (get swf-map :where)))))
  
(println (sfw-map-to-query-string {:select '("firstname" "lastname" "salary")
                          :from '("adult")
                          :where '("salary > 50" AND "firstname < \"ben\"")}))


(clojush/pushgp :error-function (fn [program]
                                  (list
                                    (let [embryo-query {:select '()
                                                        :from '()
                                                        :where '()}
                                          final-state (clojush/run-push program (clojush/push-item embryo-query
                                                                                                   :auxiliary
                                                                                                   (clojush/make-push-state)))
                                          result-query (clojush/top-item :auxiliary final-state)]
                                      ;Now, need to create a SFW string, and use it on the database to find the fitness.
                                      ;----for now, just return a random integer
                                      (rand-int 1000))))
                :atom-generators (list 'string_length
                                       'string_take
                                       'string_concat)
                :population-size 100
                :max-generations 50
                :tournament-size 7)

;;note: try using db_creation things - see if they work
(db/run-db-function db/synthesis-db db/db-query "SELECT *
                                        FROM adult
                                        WHERE age < 23 AND education = 'Masters'")