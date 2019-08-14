(ns clojush.pushgp.selection.tournament
  (:use [clojush random globals]))

(defn tournament-selection
  "Returns an individual that does the best out of a tournament."
  [pop {:keys [tournament-size total-error-method] :as argmap}]
  (let [tournament-set (doall (for [_ (range tournament-size)]
                                (lrand-nth pop)))
        err-fn (case total-error-method
                 :sum :total-error
                 (:hah :rmse :ifs) :weighted-error
                 :one-random-case (fn [ind]
                                    (nth (:errors ind)
                                         @interleaved-sampling-case-for-this-generation))
                 (throw (Exception. (str "Unrecognized argument for total-error-method: "
                                         total-error-method))))]
    (apply min-key err-fn tournament-set)))
