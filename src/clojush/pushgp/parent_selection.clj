(ns clojush.pushgp.parent-selection
  (:use [clojush.random]
        [clojush.globals]
        [clojure.math.numeric-tower])
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tournament selection
(defn tournament-selection
  "Returns an individual that does the best out of a tournament."
  [pop location {:keys [tournament-size trivial-geography-radius use-rmse
                        use-historically-assessed-hardness]}]
  (let [tournament-set 
        (doall
          (for [_ (range tournament-size)]
            (nth pop
                 (if (zero? trivial-geography-radius)
                   (lrand-int (count pop))
                   (mod (+ location (- (lrand-int (+ 1 (* trivial-geography-radius 2))) trivial-geography-radius))
                        (count pop))))))
        err-fn (cond
                 use-historically-assessed-hardness :hah-error
                 use-rmse :rms-error
                 true :total-error)]
    (reduce (fn [i1 i2] (if (< (err-fn i1) (err-fn i2)) i1 i2))
            tournament-set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexicase selection

(defn retain-one-individual-per-error-vector
  "Retains one random individual to represent each error vector."
  [pop]
  (map lrand-nth (vals (group-by #(:errors %) pop))))
  
(defn lexicase-selection
  "Returns an individual that does the best on the fitness cases when considered one at a
   time in random order.  If trivial-geography-radius is non-zero, selection is limited to parents within +/- r of location"
  [pop location {:keys [trivial-geography-radius]}]
  (let [lower (mod (- location trivial-geography-radius) (count pop))
        upper (mod (+ location trivial-geography-radius) (count pop))
        popvec (vec pop)
        subpop (if (zero? trivial-geography-radius) 
                 pop
                 (if (< lower upper)
                   (subvec popvec lower (inc upper))
                   (into (subvec popvec lower (count pop)) 
                         (subvec popvec 0 (inc upper)))))]
    (loop [survivors (retain-one-individual-per-error-vector subpop)
           cases (lshuffle (range (count (:errors (first subpop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (lrand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map #(:errors %) survivors)))]
          (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                         survivors)
                 (rest cases)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elitegroup lexicase selection

;(defn build-elitegroups
;  "Builds a sequence that partitions the cases into sub-sequences, with cases 
;grouped when they produce the same set of elite individuals in the population."
;  [pop-agents]
;  (println "Building case elitegroups...")
;  (let [pop (retain-one-individual-per-error-vector (map deref pop-agents))
;        cases (range (count (:errors (first pop))))
;        elites (map (fn [c]
;                      (let [min-error-for-case 
;                            (apply min (map #(nth % c) (map :errors pop)))]
;                        (filter #(== (nth (:errors %) c) min-error-for-case)
;                                pop)))
;                    cases)]
;    (reset! elitegroups
;            (vals (group-by #(nth elites %) cases)))
;    (println (count @elitegroups) "elitegroups:" @elitegroups)))

(defn build-elitegroups
  "Builds a sequence that partitions the cases into sub-sequences, with cases 
grouped when they produce the same set of elite individuals in the population. 
In addition, if group A produces population subset PS(A), and group B 
produces population subset PS(B), and PS(A) is a proper subset of PS(B), then 
group B is discarded. "
  [pop-agents]
  (println "Building case elitegroups...")
  (let [pop (retain-one-individual-per-error-vector (map deref pop-agents))
        cases (range (count (:errors (first pop))))
        elites (map (fn [c]
                      (let [min-error-for-case 
                            (apply min (map #(nth % c) (map :errors pop)))]
                        (filter #(== (nth (:errors %) c) min-error-for-case)
                                pop)))
                    cases)
        all-elitegroups (vals (group-by #(nth elites %) cases))
        pruned-elitegroups (filter (fn [eg]
                                     (let [e (set (nth elites (first eg)))]
                                       (not-any?
                                         (fn [eg2]
                                           (let [e2 (set (nth elites (first eg2)))]
                                             (and (not= e e2)
                                                  (set/subset? e2 e))))
                                         all-elitegroups)))
                                   all-elitegroups)]
    (reset! elitegroups pruned-elitegroups)
    (println (count @elitegroups) "elitegroups:" @elitegroups)))

(defn elitegroup-lexicase-selection
  "Returns an individual produced by elitegroup lexicase selection."
  [pop]
  (loop [survivors (retain-one-individual-per-error-vector pop)
         cases (lshuffle (map lrand-nth @elitegroups))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map #(:errors %) survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; order lexicase selection

(defn order-lexicase-selection
  "Identical to lexicase selection, except that instead of only keeping elite
   individuals at each step, keeps the best order-lexicase-retention-rate pecent
   individuals. If there is a tie at the boundry, keeps all individuals that tie.
   For example, if order-lexicase-retention-rate == 0.5, then at each step we
   keep any individual with error at least as good as the median individual on
   that test case."
  [pop location {:keys [trivial-geography-radius order-lexicase-retention-rate]}]
  (let [lower (mod (- location trivial-geography-radius) (count pop))
        upper (mod (+ location trivial-geography-radius) (count pop))
        popvec (vec pop)
        subpop (if (zero? trivial-geography-radius)
                 pop
                 (if (< lower upper)
                   (subvec popvec lower (inc upper))
                   (into (subvec popvec lower (count pop))
                         (subvec popvec 0 (inc upper)))))]
    (loop [survivors (retain-one-individual-per-error-vector subpop)
           cases (lshuffle (range (count (:errors (first subpop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (lrand-nth survivors)
        (let [kth-order-statistic (dec (round (* (count survivors) order-lexicase-retention-rate)))
              kth-err-for-case (nth (sort (map #(nth % (first cases))
                                               (map #(:errors %) survivors)))
                                    kth-order-statistic)]
          (recur (filter #(<= (nth (:errors %) (first cases)) kth-err-for-case)
                         survivors)
                 (rest cases)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parent selection

(defn select
  "Returns a selected parent."
  [pop location {:keys [use-lexicase-selection use-elitegroup-lexicase-selection
                        use-order-lexicase-selection]
                 :as argmap}]
  (cond 
    use-lexicase-selection (lexicase-selection pop location argmap)
    use-elitegroup-lexicase-selection (elitegroup-lexicase-selection pop)
    use-order-lexicase-selection (order-lexicase-selection pop location argmap)
    :else (tournament-selection pop location argmap))) ;; use tournament selection by default
