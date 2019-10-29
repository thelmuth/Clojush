(ns clojush.pushgp.counterexample-driven-gp
  (:use [clojush random globals]))


(defn check-if-all-correct-and-update-sub-cases-if-not
  ""
  [best {:keys [counterexample-driven-case-generator counterexample-driven-case-checker
                training-cases] :as argmap}]
  (let [all-cases (case counterexample-driven-case-generator
                    :hard-coded training-cases
                    :else (throw (str "Unrecognized option for :counterexample-driven-case-generator: "
                                      counterexample-driven-case-generator)))
        best-results-on-all-cases (run-best-on-all-cases best all-cases argmap)]

    ; needs to return true/false, and update sub-training-cases if false
    (case counterexample-driven-case-checker
      :automatic (counterexample-check-results-automatic best best-results-on-all-cases)
      :human (counterexample-check-results-automatic best best-results-on-all-cases))))


(defn check-counterexample-driven-results
  "Returns true if a program has been found that passes all possible training
  cases, and false otherwise.

  Needs to:
  - check if best passes all training cases
   - if so, generate new cases and have checker check if all correct
    - if so, return true (that program is a success)
    - else, have checker pick case that has wrong answer, have checker give right answer, and add that case to sub-training-cases. return false
   - else, return false"
  [population best {:keys [error-threshold] :as argmap}]
  (if (> (:total-error best) error-threshold)
    false ; This case handles best individuals that don't pass all current tests
    (check-if-all-correct-and-update-sub-cases-if-not best argmap)))
