(ns clojush.pushgp.selection.tournament
  (:use [clojush random])
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn tournament-selection
  "Returns an individual that does the best out of a tournament."
  [pop {:keys [tournament-size total-error-method
               csv-case-usage-filename generation] :as argmap}]
  (let [tournament-set (doall (for [_ (range tournament-size)]
                                (lrand-nth pop)))
        err-fn (case total-error-method
                 :sum :total-error
                 (:hah :rmse :ifs) :weighted-error
                 (throw (Exception. (str "Unrecognized argument for total-error-method: "
                                         total-error-method))))
        selected-ind (apply min-key err-fn tournament-set)]
    (with-open [csv-file (io/writer csv-case-usage-filename :append true)]
      (csv/write-csv csv-file
                     [[generation
                       0
                       (:rank-by-total-error selected-ind)]]))
    selected-ind))
