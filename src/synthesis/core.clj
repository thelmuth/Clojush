;; synthesis.core.clj
;; The main code for query creation.
;; Tom Hemuth, thelmuth@cs.umass.edu, 2011

(ns synthesis.core
  (:require [clojure.contrib.sql :as sql]
            [clojush]
            [synthesis.db :as db]))

;;;;;;;;;;;;
;; A few things must be done in the clojush namespace.
(in-ns 'clojush)

;; Redefine push-types to include :select, :from, and :where, and then redefine the push state structure.
(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :zip :string :select :from :where))
(define-push-state-structure)

;;;;;;;;;;;;
;; Return to core namespace
(in-ns 'synthesis.core)

;;;;;;;;;;
;; Globals for creating constraints

(def comparators ["=" "<" ">" "<=" ">=" "<>"])
(def cols-map db/synthesis-db-columns-map)

;;;;;;;;;;
;; Normal stack instructions for SFW
(define-registered where_dup (duper :where))
(define-registered where_swap (swapper :where))
(define-registered where_rot (rotter :where))
; Other possibilities: pop, flush, eq, stackdepth, yank, yankdup, shove

;;;;;;;;;;
;; Instructions for :where stack manipulation

(defn select-column
  "Returns column name based on index. Index is constrained within bounds of vector length by modulus."
  [index]
  (first (nth db/synthesis-db-columns (mod index (count db/synthesis-db-columns)))))

(defn get-column-type
  "Retrieves the column type for column."
  [column]
  (let [type (get cols-map column)]
    (cond
      (= type :int) :integer
      (string? type) (if (= "varchar" (subs type 0 7))
                       :string
                       nil)
      true nil)))

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
  
; Off integer stack we get (from top (0) to bottom (n)):
; 0. col-num
; 1. comparator number
; 2. (maybe) a constant to compare with
(clojush/define-registered and_constraint
                           (fn [state]
                             (if (>= (count (:integer state)) 3) ;this won't be right
                               (let [query (clojush/stack-ref :auxiliary 0 state)
                                     column (select-column (clojush/stack-ref :integer 0 state))
                                     column-type (get-column-type column)]
                                 (if (not (empty? (get state column-type))) ;is this right? yes for stacks besides :integer
                                   (let [constraint (str (name column)
                                                         " "
                                                         (nth comparators
                                                              (mod (clojush/stack-ref :integer 1 state)
                                                                   (count comparators)))
                                                         " "
                                                         (clojush/stack-ref :integer 2 state))]
                                     (clojush/push-item (add-constraint query constraint 'AND)
                                                        :auxiliary
                                                        (clojush/pop-item :auxiliary state)));trh need to pop integer or other stack
                                   state) 
                                 state))))


                           



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

#_(clojush/pushgp :error-function (fn [program]
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
#_(db/run-db-function db/synthesis-db db/db-query "SELECT *
                                                 FROM adult
                                                 WHERE age < 23 AND education = 'Masters'")

;; Test sfw-map-to-query-string
(def ex-query
  (sfw-map-to-query-string {:select ["age" "education" "hours_per_week"]
                            :from ["adult"]
                            :where ["age > 50" 'AND "workclass = \"State-gov\""]}))

#_(db/run-db-function db/synthesis-db db/db-query ex-query)

#_(add-constraint {:select ["age" "education" "hours_per_week"]
                 :from ["adult"]
                 :where ["age > 50" 'AND "workclass = \"State-gov\""]}
                "age < 25" 'AND)


;; Test and_constraint
#_(println (clojush/run-push '(2999 16 71 and_constraint)
                           (clojush/push-item {:select [] :from [] :where ["age > 50"]}
                                              :auxiliary
                                              (clojush/make-push-state))))
