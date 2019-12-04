(ns clojush.random-individual-search
  (:require [clojush.pushgp.record :as r]
            [clj-random.core :as random])
  (:use [clojush args translate evaluate globals]
        [clojush.pushgp report genetic-operators]))


(defn push-random-individual-search
  "Conducts a search by generating random individuals, testing each until one is found that passes all training cases."
  [args]
  (load-push-argmap args)
  (reset-globals)
  (initial-report @push-argmap)
  (print-params (r/config-data! [:argmap] (dissoc @push-argmap :run-uuid)))
  (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println ";; Starting generation of individuals...\n")
  (let [argmap (assoc @push-argmap :max-genome-size-in-initial-program
                      (/ (:max-points @push-argmap) 4))
        rand-gen (random/make-mersennetwister-rng (random/lrand-bytes (:mersennetwister random/*seed-length*)))]
    (loop [n 0
           best {:total-error 10e100}]
      (when (= 0 (mod n 100))
        (println)
        (println "Evaluating individual" n)
        (println "Executions used:" @program-executions-count)
        (println "Best error so far:" (:total-error best)))
      (if (> @program-executions-count (:max-program-executions argmap))
        (do (println "FAILURE")
            (println "Best found individual:" best))
        (let [ind-genome (genesis argmap)
              ind-program (assoc ind-genome :program
                                 (case (:genome-representation argmap)
                                   :plush (translate-plush-genome-to-push-program ind-genome argmap)
                                   :plushy (translate-plush-genome-to-push-program
                                            {:genome (translate-plushy-to-plush ind-genome)}
                                            argmap)))
              individual (evaluate-individual ind-program
                                              (:error-function argmap)
                                              rand-gen
                                              argmap)]
          (if (= (:total-error individual) 0)
            (do
              (println "SUCCESS")
              (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
              (prn individual))
            (recur (inc n)
                   (min-key :total-error best individual))))))))
