;; synthesis.core.clj
;; The main code for query creation.
;; Tom Hemuth, thelmuth@cs.umass.edu, 2011

(ns synthesis.core
  (:require [clojure.contrib.sql :as sql]
            [clojush]
            [synthesis.db_creation :as db]))


;;;;;;;;;;
;; Vectors for creating constraints

(def comparators ["=" "<" ">" "<=" ">=" "<>"])

;;;;;;;;;;
;; Instructions for Query from Examples

(defn add-constraint
  "Adds constraint to where clause of query."
  [query constraint logical-operator]
  (let [where (get query :where)]
    (assoc query :where
           (if (empty? where)
             (conj where constraint)
             (conj
               (conj where logical-operator)
               constraint)))))
  
  
(clojush/define-registered and_constraint
                           (fn [state]
                             (if (not (empty? (rest (:integer state))))
                               (let [query (clojush/stack-ref :auxiliary 0 state)
                                     constraint (str "age" ;trh should actually get a column here
                                                     " "
                                                     (nth comparators
                                                          (mod (clojush/stack-ref :integer 0 state)
                                                               (count comparators)))
                                                     " "
                                                     (clojush/stack-ref :integer 1 state))]
                                 (clojush/push-item (add-constraint query constraint 'AND)
                                                    :auxiliary
                                                    (clojush/pop-item :auxiliary state))) ;trh need to pop integer
                               state)))




;;;;;;;;;;
;; Query from Examples
;;
;; SFW maps are of the following form:
;; :select ["column1" "column2" "column3" ... "columnN"]
;; :from ["table1" "table2" ... "tableM"]
;; :where ["clause1" 'AND/'OR "clause2" 'AND/'OR ... "clauseZ"]

(defn sfw-map-to-query-string
  "Takes a map of a SFW query and returns a string representation of that query."
  [swf-map]
  (str "SELECT " (apply str (interpose ", " (get swf-map :select))) \newline
       "FROM " (apply str (interpose ", " (get swf-map :from))) \newline
       "WHERE " (apply str (interpose " " (get swf-map :where)))))

(clojush/pushgp :error-function (fn [program]
                                  (list
                                    (let [embryo-query {:select []
                                                        :from []
                                                        :where []}
                                          final-state (clojush/run-push program
                                                                        (clojush/push-item embryo-query
                                                                                           :auxiliary
                                                                                           (clojush/make-push-state)))
                                          result-query (clojush/top-item :auxiliary final-state)]
                                      ;Now, need to create a SFW string, and use it on the database to find the fitness.
                                      ;----for now, just return a random integer
                                      (rand-int 1000))))
                :atom-generators (list 'string_length
                                       'string_take
                                       'string_concat
                                       'and_constraint)
                :population-size 100
                :max-generations 50
                :tournament-size 7)

;;note: try using db_creation things - see if they work
(db/run-db-function db/synthesis-db db/db-query "SELECT *
                                                 FROM adult
                                                 WHERE age < 23 AND education = 'Masters'")

;; Test sfw-map-to-query-string
(def ex-query
  (sfw-map-to-query-string {:select ["age" "education" "hours_per_week"]
                            :from ["adult"]
                            :where ["age > 50" 'AND "workclass = \"State-gov\""]}))

(db/run-db-function db/synthesis-db db/db-query ex-query)

(add-constraint {:select ["age" "education" "hours_per_week"]
                 :from ["adult"]
                 :where ["age > 50" 'AND "workclass = \"State-gov\""]}
                "age < 25" 'AND)


;; Test and_constraint
(println (clojush/run-push '(2999 16 and_constraint)
                           (clojush/push-item {:select [] :from [] :where ["age > 50"]} :auxiliary (clojush/make-push-state))))
