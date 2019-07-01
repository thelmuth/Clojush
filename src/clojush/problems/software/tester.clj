(ns clojush.problems.software.tester
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower]
        ))

(defn solve
  [in]
  (loop [decimal (round-to-n-decimal-places (mod in 1) 10) binary ""]
    (cond
      (= decimal 0.0) (str (Integer/toString in 2) "." binary)
      (= (count binary) 20) (str (Integer/toString in 2) "." binary)
      (>= (* 2 decimal) 1.0) (recur (dec (* 2 decimal)) (str binary "1"))
      :else (recur (* 2 decimal) (str binary "0")))))

  ;(Integer/toString in 2)) ; gets the non-decimal

