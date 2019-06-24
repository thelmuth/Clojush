(ns clojush.problems.software.test
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

(def function-list
  (registered-for-stacks [:vector_vector_float]))

(defn random-vector
  []
  (vec (take (rand-int 11) (repeatedly #(rand 5)))))

(defn make-program
  []
  (conj
    (repeatedly (rand-int 6) #(rand 5))
    (rand-nth function-list)
    (vec (repeatedly (rand-int 11) random-vector))))

      
(defn generate
  []
  (repeatedly 1000000 #(run-push (make-program) (make-push-state))))
