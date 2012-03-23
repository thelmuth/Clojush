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
  "Takes positive examples and negative examples and returns a query that
   bounds the positive examples."
  [positive-examples negative-examples]
  (let [attributes (map first db/synthesis-db-columns)]
    (map #(attribute-bounds positive-examples %) attributes)))

(naive-bounding-box et/pos-ex et/neg-ex)
