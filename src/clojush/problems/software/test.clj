(ns clojush.problems.software.test
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

(def function-list
  (registered-for-stacks [:vector_vector_string]))

(defn random-vector
  []
  (vector (apply str (repeatedly 10 #(char (+ 97 (rand-int 26)))))))

(defn random-string
  []
  (apply str (repeatedly 10 #(char (+ 97 (rand-int 26))))))

(defn make-program
  []
  (conj
    (repeatedly (rand-int 6) #(random-string))
    (rand-nth function-list)
    (vec (repeatedly (rand-int 11) random-vector))))


(defn generate
  []
  (repeatedly 1000000 #(run-push (make-program) (make-push-state))))
