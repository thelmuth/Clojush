(ns synthesis.evaluate_examples
  (:require [clojush]
            [synthesis.core :as synth-core]
            [synthesis.examples_tables :as et]
            [synthesis.qfe :as qfe]))


;;;;;;;;;;
;; Evaluate a random invidual

#_(clojush/evaluate-individual (clojush/make-individual
                               :program (clojush/random-code 150 synth-core/qfe-atom-generators))
                             synth-core/qfe-error-function
                             (new java.util.Random))

#_(clojush/evaluate-individual (clojush/make-individual :program '(integer_add (((((integer_mult where_or) integer_sub "k" ((integer_swap integer_add) (integer_stackdepth integer_rot))) (string_length ((string_stackdepth (where_constraint_from_index))) where_constraint_from_index integer_stackdepth (integer_rot integer_mod)) ((integer_div integer_swap where_constraint_from_index where_and where_constraint_distinct_from_index (where_or)) (integer_mult) (string_stackdepth (integer_rot) string_length where_constraint_from_index integer_mult string_length integer_dup where_not) 2988)) (integer_swap integer_mod) integer_stackdepth) "Q4sEMIp" where_constraint_distinct_from_index ((integer_rot (integer_rot)) where_constraint_from_index) where_and)))
                             synth-core/qfe-error-function
                             (new java.util.Random))



;; Evaluate using qfe.clj
(def qfe-error
  (qfe/qfe-error-function-creator et/pos-ex et/neg-ex))

(et/drop-examples-table)
(et/create-and-populate-examples-table et/pos-ex et/neg-ex)


(clojush/evaluate-individual (clojush/make-individual :program '(integer_add integer_swap (integer_mult where_constraint_from_index (integer_add)) string_stackdepth ((integer_mod) ((integer_div (string_stackdepth "D" where_or (integer_swap)) where_constraint_from_index where_constraint_distinct_from_index) (where_and integer_add) 15239) (integer_swap integer_dup) integer_stackdepth where_and (integer_stackdepth)) (((integer_add (where_constraint_distinct_from_index)) where_or (("U8rO") (integer_stackdepth integer_add (integer_dup)) ("U8rO") integer_dup) integer_sub (where_constraint_distinct_from_index) ((where_and ("D" (integer_rot) (where_and where_or where_or ((where_constraint_from_stack) 18153)) integer_dup) integer_add) integer_mult integer_div integer_add) 29551) integer_mult (where_constraint_from_index integer_add) ((where_constraint_from_stack) ((integer_sub) integer_mod)) ((integer_dup) ((integer_div where_constraint_from_stack integer_div) integer_add (integer_sub)) 92 integer_sub integer_mult integer_stackdepth (integer_rot integer_dup))) (8 49 (34553) (integer_stackdepth) ((integer_stackdepth) where_or (((where_constraint_distinct_from_index (string_stackdepth)) integer_rot ((integer_swap) where_or)) (string_length) integer_add) (integer_div))))
                                                      )
                             qfe-error
                             (new java.util.Random))