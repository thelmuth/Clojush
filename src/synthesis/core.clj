;; synthesis.core.clj
;; The main code for query creation.
;; Tom Hemuth, thelmuth@cs.umass.edu, 2011

(ns synthesis.core
  (:require [clojure.contrib.sql :as sql]))

;;;;;;;;;;
;; Query creation.

(pushgp :error-function (fn [program]
                          (list
                            (let [embryo-query {:select '()
                                          :from '()
                                          :where '()}
                                  final-state (run-push program (push-item embryo-query :auxiliary (make-push-state)))
                                  result-query (top-item :auxiliary final-state)]
                              ;Now, need to create a SFW string, and use it on the database to find the fitness.
                              ;----for now, just return a random integer
                              (rand-int 1000))))
        :atom-generators (list 'string_length
                               'string_take
                               'string_concat)
        :population-size 100
        :max-generations 50
        :tournament-size 7)
