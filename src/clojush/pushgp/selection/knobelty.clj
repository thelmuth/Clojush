(ns clojush.pushgp.selection.knobelty
  (:use [clojush random]
        [clojush.pushgp.selection novelty lexicase])
  (:require [clojure.math.numeric-tower :as math]))

;; Note: The knobelty paper uses some non-standard archive and novelty
;; metric methods. We will just use the standard novelty metrics and
;; add one individual to the archive each generation, as with our
;; novelty search.

(defn knobelty-selection
  [pop argmap]
  (if (< (lrand) (:knobelty-novelty-probability argmap))
    (novelty-tournament-selection pop argmap)
    (lexicase-selection pop argmap)))
