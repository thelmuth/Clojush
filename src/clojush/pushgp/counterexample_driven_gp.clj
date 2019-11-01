(ns clojush.pushgp.counterexample-driven-gp
  (:use [clojush random args pushstate interpreter globals individual]))

; NOTE: When using counterexample-driven GP, only uses the current set of training
;       cases when simplifying at the end of a run. While intentional for now,
;       we may decide later that it would be preferable to use all available cases
;       for this simplification, which may result in better simplification.

(defn counterexample-check-results-automatic
  "Checks if the best program passed all generated cases, returning true
  if so and false otherwise. If not, also updates :sub-training-cases by
  adding a wrong case.
  This version is automatic, using the known right answers."
  [all-cases best-results-on-all-cases]
  (let [wrong-cases (remove #(= % :right-answer-on-case)
                            (map (fn [case best-result]
                                   (if (= (last case) best-result)
                                     :right-answer-on-case
                                     case))
                                 all-cases
                                 best-results-on-all-cases))]
    (if (empty? wrong-cases)
      :passes-all-cases ; program passes all generated cases
      (let [counterexample-case-to-add (lrand-nth wrong-cases)]
        ; add case to sub-training-cases
        (println "Adding case to sub-training-cases:" (pr-str counterexample-case-to-add))
        ;(print "Press enter to continue...") (flush) (read-line)
        counterexample-case-to-add)))) ; return counterexample since program does not pass all generated cases

(defn counterexample-check-results-human
  "Checks if the best program passed all generated cases, returning true
  if so and false otherwise. If not, also updates :sub-training-cases by
  adding a wrong case.
  This version uses human interaction to determine if any cases are wrong
  and pick a wrong one if so.

  NOTE WHEN IMPLEMENTING: Should print all case inputs and best outputs,
  numbered, and have user enter the number of a wrong case or correct if
  they are all correct."
  [all-cases best-results-on-all-cases]
  :stub)


(defn run-best-on-all-cases
  "Runs the program best on all generated cases, and returns a list of the
  behaviors/results of the program on those cases."
  [best all-cases {:keys [output-stacks] :as argmap}]
  (doall (for [[input correct-output] all-cases]
           (let [inputs (if (coll? input)
                          input
                          (list input))
                 start-state (reduce (fn [push-state in]
                                       (push-item in :input push-state))
                                     (push-item "" :output (make-push-state))
                                     (reverse inputs))
                 final-state (run-push (:program best)
                                       start-state)]
                                        ; Need to handle it this way for problems with more than one output.
                                        ; Note: will break if problem requires multiple outputs from the same stack.
             (if (coll? output-stacks)
               (vec (map #(top-item % final-state)
                         output-stacks))
               (top-item output-stacks final-state))))))

(defn check-if-all-correct-and-update-sub-cases-if-not
  "Finds the best program's behavior on all generated cases and checks if all outputs
  are correct with the given case checker.
  Returns solution individual if there is one.
  Returns set of new counterexample cases if not a solution."
  [sorted-pop {:keys [counterexample-driven-case-generator counterexample-driven-case-checker
                      training-cases error-threshold error-function] :as argmap}]
  (let [all-cases (case counterexample-driven-case-generator
                    :hard-coded training-cases
                    :else (throw (str "Unrecognized option for :counterexample-driven-case-generator: "
                                      counterexample-driven-case-generator)))]
    (loop [best (first sorted-pop)
           pop (rest sorted-pop)
           new-cases '()]
      (let [best-results-on-all-cases (run-best-on-all-cases best all-cases argmap)
            counterexample-case (case counterexample-driven-case-checker
                                  :automatic (counterexample-check-results-automatic
                                              all-cases best-results-on-all-cases)
                                  :human (counterexample-check-results-human
                                          all-cases best-results-on-all-cases))]
        (when (some #{counterexample-case} (:sub-training-cases @push-argmap))
          (println "Houston, we have a problem. This case is already in the training cases, and has been passed by this program.")
          (println (:sub-training-cases @push-argmap))
          (println "new case: " counterexample-case)
          (println best)
          (println "run it on new case:" (run-best-on-all-cases best (list counterexample-case) argmap))
          (throw (Exception. "Added a new case already in training cases. See above.")))
        (cond
          ; Found a solution, return it
          (= counterexample-case :passes-all-cases)
          best
          ; Didn't find a solution; if rest of population is empty, return new-cases
          (empty? pop)
          (conj new-cases counterexample-case)
          ; If there's more pop, see if next program also has 0 on training error, including
          ; on new-cases and the just-added case, which is first in :sub-training-cases.
          ; If so, recur
          (and (<= (+' (:total-error (first pop))) error-threshold)
               (<=
                (apply +'
                       (:errors (error-function (first pop)
                                                (conj new-cases (first (:sub-training-cases @push-argmap))))))
                error-threshold))
          (recur (first pop)
                 (rest pop)
                 (conj new-cases counterexample-case))
          ; If here, no more individuals with 0 training error, so return false
          :else
          (conj new-cases counterexample-case))))))


(defn check-counterexample-driven-results
  "Returns true if a program has been found that passes all generated training
  cases, and false otherwise.

  Needs to:
  - check if any programs pass all training cases
   - if so, generate new cases and have checker check if any program passes all of those
    - if so, return that program as a success
    - else, have checker pick case that has wrong answer, have checker give right answer, and add that case to sub-training-cases. return false
   - else, return false"
  [sorted-pop {:keys [error-threshold] :as argmap}]
  (if (> (:total-error (first sorted-pop)) error-threshold)
    false ; This case handles best individuals that don't pass all current tests

    ;(check-if-all-correct-and-update-sub-cases-if-not best argmap) ;; remove this line and uncomment false above
    (let [best-or-new-cases (check-if-all-correct-and-update-sub-cases-if-not sorted-pop argmap)]
      (if (= (type best-or-new-cases) clojush.individual.individual)
        best-or-new-cases ; if an individual, it is a success, so return it
        ; Otherwise, add in the new cases, and return false.
        (do
          (swap! push-argmap (fn [current-argmap] ; if cases, concat them to old cases
                               (assoc current-argmap
                                      :sub-training-cases
                                      (concat (distinct best-or-new-cases)
                                              (:sub-training-cases current-argmap)))))
          false)))))
