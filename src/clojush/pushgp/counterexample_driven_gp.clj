(ns clojush.pushgp.counterexample-driven-gp
  (:use [clojush random args pushstate interpreter]))



(defn counterexample-check-results-automatic
  "Checks if the best program passed all generated cases, returning true
  if so and false otherwise. If not, also updates :sub-training-cases by
  adding a wrong case.
  This version is automatic, using the known right answers."
  [best all-cases best-results-on-all-cases]
  (let [wrong-cases (remove #(= % :right-answer-on-case)
                            (map (fn [case best-result]
                                   (if (= (last case) best-result)
                                     :right-answer-on-case
                                     case))
                                 all-cases
                                 best-results-on-all-cases))]
    (println "================= BEGIN TRANSMITION ================")
    (prn "all cases" all-cases)
    (println)
    (prn "best results on all cases" best-results-on-all-cases)
    (println)
    (prn "wrong cases" wrong-cases)
    (println "================ END TRANSMITION ===================")
    (if (empty? wrong-cases)
      true ; program passes all generated cases
      (let [case-to-add (lrand-nth wrong-cases)]
        ; add case to sub-training-cases
        (println "Adding case to sub-training-cases:" (pr-str case-to-add))
        (swap! push-argmap (fn [argmap]
                             (assoc argmap
                                    :sub-training-cases
                                    (conj (:sub-training-cases argmap) case-to-add))))
        ;(print "Press enter to continue...") (flush) (read-line)
        
        false)))) ; return false since program does not pass all generated cases

(defn counterexample-check-results-human
  "Checks if the best program passed all generated cases, returning true
  if so and false otherwise. If not, also updates :sub-training-cases by
  adding a wrong case.
  This version uses human interaction to determine if any cases are wrong
  and pick a wrong one if so.

  NOTE WHEN IMPLEMENTING: Should print all case inputs and best outputs,
  numbered, and have user enter the number of a wrong case or correct if
  they are all correct."
  [best all-cases best-results-on-all-cases]
  :stub)


(defn run-best-on-all-cases
  "Runs the program best on all generated cases, and returns a list of the
  behaviors/results of the program on those cases."
  [best all-cases {:keys [output-stacks] :as argmap}]
  (for [[input correct-output] all-cases]
    (let [inputs (if (coll? input)
                   input
                   (list input))
          start-state (reduce (fn [push-state in]
                                (push-item in :input push-state))
                              (push-item "" :output (make-push-state))
                              inputs)
          final-state (run-push (:program best)
                                start-state)]
      ; Need to handle it this way for problems with more than one output.
      ; Note: will break if problem requires multiple outputs from the same stack.
      (if (coll? output-stacks)
        (vec (map #(top-item % final-state)
                  output-stacks))
        (top-item output-stacks final-state)))))


(defn check-if-all-correct-and-update-sub-cases-if-not
  "Finds the best program's behavior on all generated cases and checks if all outputs
  are correct with the given case checker."
  [best {:keys [counterexample-driven-case-generator counterexample-driven-case-checker
                training-cases] :as argmap}]
  (let [all-cases (case counterexample-driven-case-generator
                    :hard-coded training-cases
                    :else (throw (str "Unrecognized option for :counterexample-driven-case-generator: "
                                      counterexample-driven-case-generator)))
        best-results-on-all-cases (run-best-on-all-cases best all-cases argmap)]
    (case counterexample-driven-case-checker
      :automatic (counterexample-check-results-automatic best all-cases best-results-on-all-cases)
      :human (counterexample-check-results-human best all-cases best-results-on-all-cases))))


(defn check-counterexample-driven-results
  "Returns true if a program has been found that passes all generated training
  cases, and false otherwise.

  Needs to:
  - check if best passes all training cases
   - if so, generate new cases and have checker check if all correct
    - if so, return true (that program is a success)
    - else, have checker pick case that has wrong answer, have checker give right answer, and add that case to sub-training-cases. return false
   - else, return false"
  [best {:keys [error-threshold] :as argmap}]
  (if (> (:total-error best) error-threshold)
    false ; This case handles best individuals that don't pass all current tests

    ;(check-if-all-correct-and-update-sub-cases-if-not best argmap) ;; remove this line and uncomment false above
    (check-if-all-correct-and-update-sub-cases-if-not best argmap)))
