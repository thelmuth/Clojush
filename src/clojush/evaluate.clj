(ns clojush.evaluate
  (:use [clojush util pushstate random globals individual meta-errors]
        clojush.pushgp.genetic-operators)
  (:require [clojure.math.numeric-tower :as math]
            [clj-random.core :as random]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate the solution rates (only called from pushgp)

(defn calculate-hah-solution-rates
  [pop-agents {:keys [total-error-method error-threshold population-size]}]
  (when (= total-error-method :hah)
    (reset! solution-rates
            (let [error-seqs (map :errors (map deref pop-agents))
                  num-cases (count (first error-seqs))]
              (doall (for [i (range num-cases)]
                       (/ (count (filter #(<= % error-threshold)
                                         (map #(nth % i) error-seqs)))
                          population-size)))))
    (printf "\nSolution rates: ")
    (println (doall (map float @solution-rates)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluate individuals

(defn compute-total-error
  [errors]
  (reduce +' errors))

(defn compute-root-mean-square-error
  [errors]
  (math/sqrt (/ (apply +' (map #(* % %)
                               errors))
                (count errors))))

(defn compute-hah-error
  [errors]
  (reduce +' (doall (map (fn [rate e] (*' (- 1.01 rate) e))
                         @solution-rates
                         errors))))

(defn normalize-errors
  "Normalizes errors to [0,1] if normalize isn't :none."
  [errors normalization max-error]
  (if (= normalization :none)
    errors
    (map (fn [err]
           (case normalization
             :divide-by-max-error (double (if (>= err max-error)
                                            1.0
                                            (/ err max-error)))
             :e-over-e-plus-1 (double (/ err (inc err)))
             :min-max-scaling err ; Need population min and max, calculate later
             (throw (Exception. (str "Unrecognized argument for normalization: "
                                     normalization)))))
         errors)))

(defn normalize-by-min-max-scaling
  "Performs min-max scaling normalization after the population has been
  evaluated. ind is returned with its :errors list reset to the list
  of normalized errors. For error e, minimum of all errors min(e), etc.:
     normalized_e = ((e - min(e)) / (max(e) - min(e)))
  see https://sebastianraschka.com/Articles/2014_about_feature_scaling.html#about-min-max-scaling"
  [ind error-mins error-maxs]
  (assert (not (empty? (:errors ind))))
  (assert (not (empty? error-mins)))
  (assert (not (empty? error-maxs)))
  (assoc ind
         :errors
         (map (fn [error error-min error-max]
                                        ; If error-min = error-max, entire population has same error
                                        ; on this case, so it doesn't matter what the normalized
                                        ; error is. Return 0 in this case.
                (if (= error-min error-max)
                  0
                  (/ (- error error-min)
                     (- error-max error-min))))
              (:errors ind)
              error-mins
              error-maxs)))

(defn evaluate-individual
  "Returns the given individual with errors, total-errors, and weighted-errors,
   computing them if necessary."
  ([i error-function rand-gen]
    (evaluate-individual i error-function rand-gen
                         {:reuse-errors true
                          :print-history false
                          :total-error-method :sum
                          :normalization :none
                          :max-error 1000}))
  ([i error-function rand-gen
    {:keys [reuse-errors print-history total-error-method normalization max-error
            parent-selection]
     :as argmap}]
    (random/with-rng rand-gen
      (let [p (:program i)
            evaluated-i (cond
                          (and reuse-errors (not (nil? (:errors i))))
                          i
                          ;;
                          (= parent-selection :downsampled-lexicase)
                          (error-function i (:sub-training-cases argmap))
                          ;;
                          :else
                          (error-function i))
            raw-errors (:errors evaluated-i)
            e (vec (if (and reuse-errors (not (nil? (:errors i))))
                     (:errors i)
                     (do
                       (swap! evaluations-count inc)
                       (normalize-errors raw-errors normalization max-error))))
            te (if (and reuse-errors (not (nil? (:total-error i))))
                 (:total-error i)
                 (compute-total-error raw-errors))
            ne (if (and reuse-errors (not (nil? (:normalized-error i))))
                 (:normalized-error i)
                 (compute-total-error e))
            co-solve (when (= total-error-method :co-solvability)
                       (for [x (range (count e))
                             y (range (count e))
                             :when (< x y)]
                         (/ (+ (- 1.0 (nth e x)) (- 1.0 (nth e y)))
                            2)))
            we (case total-error-method
                 :sum nil
                 :ifs nil ; calculated later
                 :co-solvability nil ; calculated later
                 :eliteness nil ; calculated later
                 :hah (compute-hah-error e)
                 :rmse (compute-root-mean-square-error e)
                 nil)
            new-ind (assoc evaluated-i ; Assign errors and history to i
                           :errors e
                           :co-solvability-rewards co-solve
                           :total-error te
                           :weighted-error we
                           :normalized-error ne
                           :history (if print-history (cons e (:history i)) (:history i)))]
        new-ind))))

