;; tagged_regression.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns clojush.problems.demos.tagged-regression-c
  (:use [clojush util globals pushstate simplification individual] 
        [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]
        [clojush.instructions.tag])
  (:require [clojure.string :as string]
            [local-file]))

;;;;;;;;;;;;
;; Integer symbolic regression of x^3 - 2x^2 - x (problem 5 from the 
;; trivial geography chapter) with minimal integer instructions
;; ALSO uses tags, although there is little reason to think they would
;; help on such a simple problem.

;; Before executing the individual programs for the purpose of error calculation, 
;; initialize its tagspace by tagging the whole program with a certain number for tags.
;; Types of values to be tagged include signle instructions/literals and list of instructions/literals
;; enclosed within brackets.


;; http://www.drregex.com/2017/11/match-nested-brackets-with-regex-new.html

(def argmap
  {:error-function (fn [individual]
                     (assoc individual
                            :errors (let [state-with-tags (tagspace-initialization (str (:program individual)) 100 (make-push-state))]
                                      (doall
                                        (for [input (range 10)]
                                          (let [state (run-push (:program individual)
                                                                (push-item input :input
                                                                           (push-item input :integer state-with-tags)))
                                                top-int (top-item :integer state)]
                                            (if (number? top-int)
                                              (abs (- top-int 
                                                      (- (* input input input) 
                                                         (* 2 input input) input)))
                                              1000)))))))
   :atom-generators (list (fn [] (lrand-int 10))
                          'in1
                          'integer_div
                          'integer_mult
                          'integer_add
                          'integer_sub
                          (tag-instruction-erc [:integer :exec] 100)
                          (untag-instruction-erc 100)
                          (tagged-instruction-erc 100))
   :tag-limit 100
   :parent-selection :tournament
   :tournament-size 3
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.4
                                    :uniform-close-mutation 0.1}
   })
