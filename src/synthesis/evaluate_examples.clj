(ns synthesis.evaluate_examples
  (:require [clojush]
            [synthesis.core :as synth-core]))


;;;;;;;;;;
;; Evaluate a random invidual

#_(clojush/evaluate-individual (clojush/make-individual
                               :program (clojush/random-code 150 synth-core/qfe-atom-generators))
                             synth-core/qfe-error-function
                             (new java.util.Random))

#_(clojush/evaluate-individual (clojush/make-individual :program '(integer_add (((((integer_mult where_or) integer_sub "k" ((integer_swap integer_add) (integer_stackdepth integer_rot))) (string_length ((string_stackdepth (where_constraint_from_index))) where_constraint_from_index integer_stackdepth (integer_rot integer_mod)) ((integer_div integer_swap where_constraint_from_index where_and where_constraint_distinct_from_index (where_or)) (integer_mult) (string_stackdepth (integer_rot) string_length where_constraint_from_index integer_mult string_length integer_dup where_not) 2988)) (integer_swap integer_mod) integer_stackdepth) "Q4sEMIp" where_constraint_distinct_from_index ((integer_rot (integer_rot)) where_constraint_from_index) where_and)))
                             synth-core/qfe-error-function
                             (new java.util.Random))
