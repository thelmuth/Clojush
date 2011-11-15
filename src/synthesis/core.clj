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
(clojush/define-registered where_dup (clojush/duper :where))
(clojush/define-registered where_swap (clojush/swapper :where))
(clojush/define-registered where_rot (clojush/rotter :where))
; Other possibilities: pop, flush, eq, stackdepth, yank, yankdup, shove

;;;;;;;;;;
;; Helper functions for :where stack manipulation

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

(defn get-constant-from-column
  "Gets a constant from column from row index. Optional argument distinct is a boolean that defines whether
   or not the values in the column should be distinct. Index is taken mod the number of options to always
   give a legal index."
  [column index distinct]
  (let [query (str "SELECT "
                   (if distinct
                     "DISTINCT "
                     "")
                   (name column)
                   " FROM adult")
        results (db/run-db-function db/synthesis-db db/db-query query)
        return (get (nth results
                         (mod index (count results)))
                    column)]
    (if (string? return)
      (str "'" return "'")
      return)))

;;;;;;;;;;
;; Instructions for :where stack manipulation

(clojush/define-registered
  where_constraint_from_stack
  (fn [state]
    (if (not (>= (count (get state :integer)) 2)) ; We will need at least 2 integers
      state
      (let [column (select-column (clojush/stack-ref :integer 0 state))
            column-type (get-column-type column)]
        (if (nil? column-type) ; Check for legit column-type
          state
          (if (or (empty? (get state column-type)) ; Make sure column type isn't empty
                  (and (= column-type :integer)
                       (not (>= (count (get state :integer)) 3))))
            state
            (let [comparator (nth comparators (mod (clojush/stack-ref :integer 1 state)
                                                   (count comparators)))
                  constant (cond
                             (= column-type :integer) (clojush/stack-ref :integer 2 state)
                             (= column-type :string) (str "'"
                                                          (clojush/stack-ref :string 0 state)
                                                          "'")
                             true (clojush/stack-ref column-type 0 state))
                  constraint (str (name column) " " comparator " " constant)]
              (clojush/push-item constraint :where
                                 (clojush/pop-item :integer
                                                   (clojush/pop-item :integer
                                                                     (clojush/pop-item column-type
                                                                                       state)))))))))))

; Uses a constant taken from the selected column, where options are not distinct.
; This makes probability of choosing constant C proportional to the frequency of C in the column.
(clojush/define-registered
  where_constraint_from_index
  (fn [state]
    (if (not (>= (count (get state :integer)) 3)) ; We will need 3 integers
      state
      (let [column (select-column (clojush/stack-ref :integer 0 state))
            column-type (get-column-type column)]
        (if (nil? column-type) ; Check for legit column-type
          state
          (let [comparator (nth comparators (mod (clojush/stack-ref :integer 1 state)
                                                 (count comparators)))
                constant (get-constant-from-column column
                                                   (clojush/stack-ref :integer 2 state)
                                                   false)
                constraint (str (name column) " " comparator " " constant)]
            (clojush/push-item constraint :where
                               (clojush/pop-item :integer
                                                 (clojush/pop-item :integer
                                                                   (clojush/pop-item :integer
                                                                                     state))))))))))

; Uses a constant taken from the selected column, where options are distinct.
; This makes each distinct option equally likely.
(clojush/define-registered
  where_constraint_distinct_from_index
  (fn [state]
    (if (not (>= (count (get state :integer)) 3)) ; We will need 3 integers
      state
      (let [column (select-column (clojush/stack-ref :integer 0 state))
            column-type (get-column-type column)]
        (if (nil? column-type) ; Check for legit column-type
          state
          (let [comparator (nth comparators (mod (clojush/stack-ref :integer 1 state)
                                                 (count comparators)))
                constant (get-constant-from-column column
                                                   (clojush/stack-ref :integer 2 state)
                                                   true)
                constraint (str (name column) " " comparator " " constant)]
            (clojush/push-item constraint :where
                               (clojush/pop-item :integer
                                                 (clojush/pop-item :integer
                                                                   (clojush/pop-item :integer
                                                                                     state))))))))))

(defn where-conjoiner
  [conjunction]
  (fn [state]
    (if (not (>= (count (get state :where)) 2))
      state
      (let [first-item (clojush/stack-ref :where 0 state)
            second-item (clojush/stack-ref :where 1 state)]
        (clojush/push-item (str "(" second-item " " conjunction " " first-item ")")
                           :where
                           (clojush/pop-item :where
                                             (clojush/pop-item :where state)))))))
            
(clojush/define-registered where_and (where-conjoiner "AND"))
(clojush/define-registered where_or (where-conjoiner "OR"))

(clojush/define-registered
  where_not
  (fn [state]
    (if (empty? (get state :where))
      state
      (let [item (clojush/stack-ref :where 0 state)]
        (clojush/push-item (str "(NOT " item ")")
                           :where
                           (clojush/pop-item :where state))))))

;;;;;;;;;;
;; Helper functions for pushgp

(defn stacks-to-query-string
  "Takes a Push state including the :select, :from, and :where stacks, and returns a
   string representation of that query."
  [push-state]
  (let [select (clojush/top-item :select push-state)
        from (clojush/top-item :from push-state)
        where (clojush/top-item :where push-state)]
    (str "SELECT " select \newline
         "FROM " from \newline
         (when (not (= where :no-stack-item)) (str "WHERE " where)))))

(defn run-query-for-agent
  "Takes an agent's old return and a new query string, and runs the query on db/synthesis-db."
  [old-return query-string]
  (db/run-db-function db/synthesis-db
                      db/db-query
                      query-string))

;;;;;;;;;;
;; Query from Examples

(def qfe-atom-generators
  (concat #_(clojush/registered-for-type :where)
          (list 'where_dup
                'where_swap
                'where_rot
                'where_constraint_distinct_from_index
                'where_constraint_from_index
                'where_constraint_from_stack
                'where_and
                'where_or
                'where_not)
          (list 'string_length
                'string_take
                'string_concat
                'string_stackdepth
                'string_dup
                'string_swap
                'string_rot)
          (list 'integer_add
                'integer_sub
                'integer_mult
                'integer_div
                'integer_mod
                'integer_stackdepth
                'integer_dup
                'integer_swap
                'integer_rot)
          (list (fn [] 
                  (let [choice (clojush/lrand-int 5)]
                    (case choice
                      0 (clojush/lrand-int 10)
                      1 (clojush/lrand-int 100)
                      2 (clojush/lrand-int 1000)
                      3 (clojush/lrand-int 10000)
                      4 (clojush/lrand-int 100000))))
                (fn [] (let [chars (str "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "abcdefghijklmnopqrstuvwxyz"
                                        "0123456789")
                             chars-count (count chars)]
                         (apply str (repeatedly (+ 1 (clojush/lrand-int 9))
                                                #(nth chars (clojush/lrand-int chars-count)))))))))

(def qfe-error-function
  (fn [program]
    (list
      (let [final-state (clojush/run-push
                          program
                          (clojush/push-item "adult"
                                             :from
                                             (clojush/push-item "*"
                                                                :select
                                                                (clojush/make-push-state))))
            result-query-string (stacks-to-query-string final-state)
            query-future (future
                           (db/run-db-function db/synthesis-db
                                               db/db-query
                                               result-query-string))]
            ;query-future (future (vec '({:education "Masters"} {:education "10th"})))]
        (try
          (let [rows (count (.get query-future 2000 (java.util.concurrent.TimeUnit/MILLISECONDS)))]
            (println "---")
            (println "Query:")
            (println result-query-string)
            (println "Rows returned:" rows)
            (- 10000 (count result-query-string))) ;;for now, return 1000 - length of the string
          (catch java.util.concurrent.TimeoutException e
                 (println "---")
                 (println "Query:")
                 (println result-query-string)
                 (if (future-cancel query-future)
                   (println "future cancelled")
                   (println "future could not be cancelled"))
                 10000)))))) ;;penalty of 10000 for not returning




(clojush/pushgp
  :error-function qfe-error-function
  :atom-generators qfe-atom-generators
  :population-size 10
  :max-generations 2
  :tournament-size 7
  :report-simplifications 0
  :final-report-simplifications 0
  :reproduction-simplifications 1)



;; Evaluate a random invidual
#_(clojush/evaluate-individual (clojush/make-individual :program (clojush/random-code 150 qfe-atom-generators))
                             qfe-error-function
                             (new java.util.Random))



;; Test sfw-map-to-query-string
#_(def ex-query
    (sfw-map-to-query-string {:select ["age" "education" "hours_per_week"]
                              :from ["adult"]
                              :where ["age > 50" 'AND "workclass = \"State-gov\""]}))

#_(db/run-db-function db/synthesis-db db/db-query ex-query)

;; Test a query
#_(time (db/run-db-function db/synthesis-db
                            db/db-query
                            "SELECT DISTINCT workclass
                             FROM adult
                             WHERE (NOT workclass > 'Private')"))


;; To get an indexed thing from the distinct things
#_(nth (db/run-db-function db/synthesis-db db/db-query "SELECT DISTINCT education
                                                        FROM adult
                                                        WHERE 0=0") 8)

;; To get an indexed thing from non-distinct things
#_(nth (db/run-db-function db/synthesis-db db/db-query "SELECT education
                                                        FROM adult
                                                        WHERE 0=0") 24212)

;; Test and_constraint
#_(println (clojush/run-push '(24 "thomas" 2 1 where_constraint_from_stack)
                             (clojush/make-push-state)))

#_(println (clojush/run-push '(4 "thomas" 2 1 where_constraint_from_index where_not)
                             (clojush/make-push-state)))


#_(stacks-to-query-string (clojush/run-push '("thomas" 7 2 1 where_constraint_distinct_from_index
                                                       539 2 10 where_constraint_distinct_from_index
                                                       14 90 3 where_constraint_from_index
                                                       where_and
                                                       4 13 214 where_constraint_from_stack
                                                       where_or)
                                            (clojush/push-item "adult"
                                                               :from
                                                               (clojush/push-item "*"
                                                                                  :select
                                                                                  (clojush/make-push-state)))))

#_(clojush/registered-for-type :where)

#_(clojush/registered-for-type :integer)
