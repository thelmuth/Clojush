(ns clojush.random-individual-search
  (:require [clojush.pushgp.record :as r]
            [clj-random.core :as random])
  (:use [clojush args translate evaluate globals]
        [clojush.pushgp report genetic-operators]))


(defn num-starting-zeros-of-inds-errors
  [ind]
  (if (not= (first (:errors ind)) 0)
    0
    (count (first (partition-by #(= % 0)
                                (:errors ind))))))

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
           best {:errors '(1) :total-error 1e100}]
      (when (= 0 (mod n 10))
        (println)
        (println "Ind:" n)
        (println "Executions:" @program-executions-count)
        (println "Best zeros:" (num-starting-zeros-of-inds-errors best)))
      (if (>= @program-executions-count (:max-program-executions argmap))
        (do (println "FAILURE")
            (println "Best found individual:" best)
            )
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
          (if (empty? (remove #(= % 0) (:errors individual)))
            (do
              (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
              (println "SUCCESSFUL INDIVIDUAL")
              (prn individual)
              (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
              (println "Testing unsimplified on test data")
              ((:problem-specific-report argmap)
               (assoc individual :total-error (apply +' (:errors individual)))
               '(individual)
               n
               (:error-function argmap)
               (:final-report-simplifications argmap))
              (println "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
              (println "Simplifying and re-testing")
              (final-report n individual argmap))
            (recur (inc n)
                   (max-key num-starting-zeros-of-inds-errors
                            best individual)
                   #_(min-key :total-error
                            best individual))))))))
