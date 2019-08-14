(ns clojush.pushgp.selection.interleaved-sampling
  (:use clojush.random
        clojush.pushgp.selection.tournament))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interleaved Sampling
;; from: Balancing Learning and Overfitting in Genetic Programming with Interleaved Sampling of Training Data
;; Gonçalves, Ivo and Silva, Sara

(defn interleaved-sampling
  [pop generation argmap]
  (if (zero? (mod generation 2))
    (tournament-selection pop argmap)
    (tournament-selection pop (assoc argmap :total-error-method :one-random-case))))
