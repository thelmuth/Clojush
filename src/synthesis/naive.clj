(ns synthesis.naive
  (:require [synthesis.db :as db]
            [synthesis.examples_tables :as et]))

(defn first-string
  [a b]
  (if (> 0 (compare a b))
    a
    b))

(defn last-string
  [a b]
  (if (> 0 (compare a b))
    b
    a))

(defn attribute-bounds
  "Takes a vector of example tuples (maps) and an attribute of those maps,
   and returns the bound on that attribute over those tuples."
  [examples attribute]
  (let [values (map #(get % attribute) examples)]
    (cond
      (number? (first values)) (vector (apply min values)
                                       (apply max values))
      (string? (first values)) (vector (reduce first-string values)
                                       (reduce last-string values))
      true nil)))


(defn naive-bounding-box
  "Takes positive examples and negative examples and returns a WHERE clause that
   bounds the positive examples."
  [positive-examples negative-examples]
  (let [attributes (map first db/synthesis-db-columns)
        bounds (map #(attribute-bounds positive-examples %) attributes)]
    (str "("
         (reduce #(str %1 " AND " %2)
                 (map #(let [string-quote (if (string? (first %2))
                                            "'"
                                            "")]
                         (str "("
                              string-quote
                              (first %2)
                              string-quote
                              " <= "
                              (name %1)
                              " AND "
                              (name %1)
                              " <= "
                              string-quote
                              (second %2)
                              string-quote
                              ")"))
                      attributes
                      bounds))
         ")")))


;(naive-bounding-box et/pos-ex et/neg-ex)



;; Printout data for C5.0
(defn c50-printout
  [positive-examples negative-examples]
  (let [attributes (map first db/synthesis-db-columns)
        example-to-string (fn [example class]
                            (apply str (interpose ", "
                                                  (concat (map #(get example %) attributes)
                                                          [class]))))]
    (println (apply str
                    (interpose "\n" (concat (map example-to-string
                                                 positive-examples
                                                 (repeat "pos"))
                                            (map example-to-string
                                                 negative-examples
                                                 (repeat "neg"))))))))

(def Q0-where
  "(education_num < 9 AND hours_per_week > 40) OR (occupation = 'Farming-fishing')")

; Print cond.data
;(c50-printout et/pos-ex et/neg-ex)

; Print cond.test
#_(c50-printout (db/run-db-function db/synthesis-db
                                  db/db-query
                                  (str "SELECT *
                                        FROM adult
                                        WHERE "
                                       Q0-where))
              (db/run-db-function db/synthesis-db
                                  db/db-query
                                  (str "SELECT *
                                        FROM adult
                                        WHERE NOT("
                                       Q0-where
                                       ")")))
