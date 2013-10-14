(ns clojush.pushgp.pushgp
  (:require [clojure.java.io :as io])
  (:use [clojush globals util pushstate random individual evaluate]
        [clojush.instructions boolean code common numbers random-instructions string tag zip return]
        [clojush.pushgp breed parent-selection report]
        [clojush.experimental.decimation]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pushgp

(def push-argmap
  (atom (sorted-map :error-function (fn [p] '(0)) ;; pgm -> list of errors (1 per case)
                    :error-threshold 0
                    :top-level-push-code true
                    :top-level-pop-code true
                    :population-size 1000
                    :max-points 50 
                    :max-points-in-initial-program 50
                    :atom-generators (concat @registered-instructions
                                             (list 
                                               (fn [] (lrand-int 100))
                                               (fn [] (lrand))))
                    :max-generations 1001
                    :max-mutations nil
                    :mutation-probability 0.4
                    :mutation-max-points 20
                    :crossover-probability 0.4
                    :simplification-probability 0.1
                    :tournament-size 7
                    :report-simplifications 100
                    :final-report-simplifications 1000
                    :reproduction-simplifications 1
                    :trivial-geography-radius 0
                    :decimation-ratio 1
                    :decimation-tournament-size 2
                    :evalpush-limit 150
                    :evalpush-time-limit 0
                    :node-selection-method :unbiased
                    :node-selection-leaf-probability 0.1
                    :node-selection-tournament-size 2
                    :pop-when-tagging true
                    :gaussian-mutation-probability 0.0
                    :gaussian-mutation-per-number-mutation-probability 0.5
                    :gaussian-mutation-standard-deviation 0.1
                    :reuse-errors true
                    :problem-specific-report default-problem-specific-report
                    :use-single-thread false
                    :random-seed (System/nanoTime)
                    :use-historically-assessed-hardness false
                    :use-lexicase-selection false ; If true, uses Lexicase Parent Selection (see Spector paper in GECCO-UP 2012 workshop proceedings)
                    :use-elitegroup-lexicase-selection false
                    :use-rmse false
                    :print-csv-logs false
                    :print-json-logs false
                    :csv-log-filename "log.csv"
                    :json-log-filename "log.json"
                    :log-fitnesses-for-all-cases false
                    :json-log-program-strings false 
                    :boolean-gsxover-probability 0.0
                    :boolean-gsxover-new-code-max-points 20
                    :deletion-mutation-probability 0.0
                    :parentheses-addition-mutation-probability 0.0
                    :tagging-mutation-probability 0.0
                    :tag-branch-mutation-probability 0.0
                    :tag-branch-mutation-type-instruction-pairs []
                    :parent-reversion-probability 0.0
                    :tag-limit 10000
                    :initial-population nil
                    :ultra-probability 0.0
                    :ultra-alternation-rate 0.1
                    :ultra-alignment-deviation 1
                    :ultra-mutation-rate 0.1
                    :print-errors true
                    :print-history false
                    :print-timings false ; If true, report prints how long different parts of evolution have taken during the current run.
                    :print-cosmos-data false ; If true, report prints COSMOS data each generation.
                    :print-ancestors-of-solution false ; If true, final report prints the ancestors of the solution
                    :maintain-ancestors false  ; If true, save all ancestors in each individual (costly)
                    :save-initial-population false
                    :use-bushy-code false
                    :use-ultra-no-paren-mutation false ; If true, ULTRA will use no-paren mutation, which means that parentheses won't be added or deleted during mutation.
                    )))

(defn load-push-argmap
  [argmap]
  (doseq [[argkey argval] argmap]
    (assert (contains? @push-argmap argkey) (str "Argument key " argkey " is not a recognized argument to pushgp."))
    (swap! push-argmap assoc argkey argval)))

(defn reset-globals []
  (doseq [[gname gatom] (filter (fn [[a _]] (.startsWith (name a) "global-")) (ns-publics 'clojush.globals))]
    (when (contains? @push-argmap (keyword (.substring (name gname) (count "global-"))))
      (reset! @gatom (get @push-argmap (keyword (.substring (str gname) (count "global-"))))))))

(defn make-agents-and-rng [{:keys [initial-population use-single-thread population-size
                                   max-points-in-initial-program atom-generators random-seed
                                   save-initial-population]}]
  (let [agent-error-handler (fn [agnt except]
                              (.printStackTrace except System/out)
                              (.printStackTrace except)
                              (System/exit 0))
        random-seeds (loop [seeds '()]
                       (let [num-remaining (if initial-population
                                             (- (count initial-population) (count seeds))
                                             (- population-size (count seeds)))]
                         (if (pos? num-remaining)
                           (recur (distinct (concat seeds (repeatedly num-remaining
                                                                      #(lrand-int Integer/MAX_VALUE)))))
                           seeds)))]
    {:pop-agents (if initial-population
                   (->> (read-string (slurp (str "data/" initial-population)))
                        (map #(if use-single-thread (atom %) (agent %)))
                        (vec))
                   (let [pa (doall (for [_ (range population-size)]
                                     (make-individual
                                       :program (random-code max-points-in-initial-program atom-generators)
                                       :error-handler agent-error-handler)))
                         f (str "data/" (System/currentTimeMillis) ".ser")]
                     (when save-initial-population
                       (io/make-parents f)
                       (spit f (printable (map individual-string pa))))
                     (vec (map #(if use-single-thread (atom %) (agent %)) pa))))
     :child-agents (vec (doall (for [_ (range population-size)]
                                 ((if use-single-thread atom agent)
                                      (make-individual)
                                      :error-handler agent-error-handler))))
     :random-seeds random-seeds
     :rand-gens (vec (doall (for [k (range population-size)]                      
                              (java.util.Random. (nth random-seeds k)))))
     }))

(defn compute-errors [pop-agents rand-gens {:keys [use-single-thread error-function]
                                            :as argmap}]
  (dorun (map #((if use-single-thread swap! send)
                    % evaluate-individual error-function %2 argmap)
              pop-agents
              rand-gens))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE ;might this need a dorun?


;; I feel like the printing should be in the main loop, but i'm just cutting and pasting for now
(defn parental-reversion
  [pop-agents generation {:keys [parent-reversion-probability use-single-thread use-rmse
                                 use-historically-assessed-hardness]}]
  (if (and (> generation 0) (> parent-reversion-probability 0))
    (let [err-fn (cond
                   use-historically-assessed-hardness :hah-error
                   use-rmse :rms-error
                   :else :total-error)]
      (println "Performing parent reversion...")
      (dorun (map #((if use-single-thread swap! send) 
                        % 
                        (fn [i]  
                          (if (or (< (err-fn i) (err-fn (:parent i)))
                                  (and (= (err-fn i) (err-fn (:parent i)))
                                       (< (count-points (:program i))
                                          (count-points (:program (:parent i)))))
                                  (> (lrand) parent-reversion-probability))
                            (assoc i :parent nil)  ;; don't store whole ancestry
                            (:parent i))))
                  pop-agents))
      (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
      (println "Done performing parent reversion."))))

(defn remove-parents
  "Removes value from :parent for each individual in the population. This will
   save memory."
  [pop-agents {:keys [use-single-thread]}]
  (dorun (map #((if use-single-thread swap! send)
                    %
                    (fn [i] (assoc i :parent nil)))
              pop-agents))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

;; this is a wrapper for calculate-hah-solution-rates, which should itself be changed
(defn calculate-hah-solution-rates-wrapper 
  [pop-agents {:keys [use-historically-assessed-hardness error-threshold population-size]}]
  (calculate-hah-solution-rates use-historically-assessed-hardness pop-agents error-threshold population-size))

(defn report-and-check-for-success
  [pop-agents generation argmap]
  (let [best (report (vec (doall (map deref pop-agents))) generation argmap)]
    (cond (<= (:total-error best) (get argmap :error-threshold)) best
          (>= generation (get argmap :max-generations)) :failure
          :else :continue)))
          
(defn produce-new-offspring
  [pop-agents child-agents rand-gens
   {:keys [decimation-ratio population-size decimation-tournament-size trivial-geography-radius
           use-single-thread
           ]}]
  (let [pop (if (>= decimation-ratio 1)
              (vec (doall (map deref pop-agents)))
              (decimate (vec (doall (map deref pop-agents)))
                        (int (* decimation-ratio population-size))
                        decimation-tournament-size
                        trivial-geography-radius))]
    (dotimes [i population-size]
      ((if use-single-thread swap! send)
           (nth child-agents i) 
           breed 
           i (nth rand-gens i) pop @push-argmap)))
  (when-not use-single-thread (apply await child-agents))) ;; SYNCHRONIZE

(defn install-next-generation [pop-agents child-agents {:keys [population-size use-single-thread]}]
  (dotimes [i population-size]
    ((if use-single-thread swap! send)
         (nth pop-agents i) (fn [av] (deref (nth child-agents i)))))
  (when-not use-single-thread (apply await pop-agents))) ;; SYNCHRONIZE

(defn timer
  "Used to track the time used by different parts of evolution."
  [{:keys [print-timings]} step]
  (when print-timings
    (let [start-time @timer-atom
          current-time-for-step (get @timing-map step)]
      (reset! timer-atom (System/currentTimeMillis))
      (swap! timing-map assoc step (+ current-time-for-step (- @timer-atom start-time))))))

(defn pushgp
  "The top-level routine of pushgp."
  ([] (pushgp '()))
  ([args]
    (reset! timer-atom (System/currentTimeMillis))
    (load-push-argmap args)
    (binding [*thread-local-random-generator* (java.util.Random. (:random-seed @push-argmap))]
      ;; set globals from parameters
      (reset-globals)
      (initial-report) ;; Print the inital report
      (print-params @push-argmap)
      (timer @push-argmap :initialization)
      (println "Generating initial population...")
      (let [{:keys [pop-agents child-agents rand-gens random-seeds]} (make-agents-and-rng @push-argmap)]
        ;(print "Random seeds: ")
        ;(doseq [seed random-seeds] (print " " seed))
        ;(println)
        ;; Main loop
        (loop [generation 0]
          (println "Processing generation:" generation)
          (timer @push-argmap :reproduction)
          (print "Computing errors... ")
          (compute-errors pop-agents rand-gens @push-argmap)
          (println "Done computing errors.")
          (timer @push-argmap :fitness)
          ;; possible parent reversion
          (parental-reversion pop-agents generation @push-argmap)
          ;; remove parents since they aren't used any more
          (remove-parents pop-agents @push-argmap)
          ;; calculate solution rates if necessary for historically-assessed hardness
          ;; change calculate-hah-solution-rates in the future, to destructure the argmap
          (calculate-hah-solution-rates-wrapper pop-agents @push-argmap)
          ;; create global structure to support elitegroup lexicase selection
          (when (:use-elitegroup-lexicase-selection @push-argmap)
            (build-elitegroups pop-agents))
          (timer @push-argmap :other)
          ;; report and check for success
          (let [outcome (report-and-check-for-success pop-agents generation @push-argmap)]
            (cond (= outcome :failure) (do (printf "\nFAILURE\n") (flush))
                  (= outcome :continue) (do (timer @push-argmap :report)
                                            (println "Producing offspring...")
                                            (produce-new-offspring pop-agents child-agents rand-gens @push-argmap)
                                            (println "Installing next generation...")
                                            (install-next-generation pop-agents child-agents @push-argmap)
                                            (recur (inc generation)))
                  :else (final-report generation outcome @push-argmap))))))))
