(ns synthesis.qfe
  (:require [clojush]
            [synthesis.core :as synth-core]
            [synthesis.db :as db]))

;;;;;;;;;;
;; Error function

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
            result-query-string (synth-core/stacks-to-query-string final-state)
            query-future (future
                           (db/run-db-function db/synthesis-db
                                               db/db-query
                                               result-query-string))]
        (try
          (let [rows (count (.get query-future 1000 (java.util.concurrent.TimeUnit/MILLISECONDS)))]
            (println "---")
            (println "Query:")
            (println result-query-string)
            (println "Rows returned:" rows)
            (println "Error:" (Math/abs (- 16250 rows)))
            (Math/abs (- 16250 rows))) ;;for now, return abs(16250 - rows returned)
          (catch java.util.concurrent.TimeoutException e
                 (println "---")
                 (println "Query:")
                 (println result-query-string)
                 (if (future-cancel query-future)
                   (println "future cancelled")
                   (println "future could not be cancelled"))
                 100000)))))) ;;penalty of 100000 for not returning

;;;;;;;;;;
;; Main pushgp call

(clojush/pushgp
  :error-function qfe-error-function
  :atom-generators synth-core/qfe-atom-generators
  :max-points 250
  :evalpush-limit 300
  :population-size 100
  :max-generations 20
  :tournament-size 7
  :report-simplifications 0
  :final-report-simplifications 10
  :reproduction-simplifications 1
  :use-single-thread true)
