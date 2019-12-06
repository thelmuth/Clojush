(ns clojush.problems.test.run-random-programs
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        ))

(defn int-erc-10000
  []
  (- (lrand-int 20001) 10000))

(defn float-erc-10000
  []
  (- (* (lrand) 20000.0) 10000.0))

(defn boolean-erc
  []
  (lrand-nth (list true false)))

(defn visible-char-erc
  []
  (lrand-nth (concat [\newline \tab] (map char (range 32 127)))))

(defn string-erc
  []
  (apply str (repeatedly (lrand-int 50) (fn [] (lrand-nth (map char (range 32 127)))))))

(defn vector-boolean-erc
  []
  (let [length (rand-int 100)]
    (vec (repeatedly length boolean-erc))))

(defn vector-integer-erc
  []
  (let [length (rand-int 100)]
    (vec (repeatedly length int-erc-10000))))

(defn vector-float-erc
  []
  (let [length (rand-int 100)]
    (vec (repeatedly length float-erc-10000))))

(defn vector-string-erc
  []
  (let [length (rand-int 100)]
    (vec (repeatedly length string-erc))))

; Atom generators
(def atom-generators
  (concat (list
            (tag-instruction-erc [:exec :integer :boolean :float :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tagERCs
            'in1
            'in2
            ;;; end input instructions
            )

                    ;; Kitchen sink ERCs
          (list
           (fn [] (- (lrand-int 257) 128)) ;Integer ERC [-128,128]
           int-erc-10000 ;Integer ERC [-10000,10000]
           (fn [] (- (* (lrand) 1000.0) 500.0)) ;Float ERC [-500.0,500.0)
           float-erc-10000 ;Float ERC [-10000.0,10000.0)
           boolean-erc ;Boolean ERC
           visible-char-erc ;Visible character ERC
           string-erc ;String ERC
           vector-boolean-erc
           vector-string-erc
           vector-integer-erc
           vector-float-erc
           )
          ;; Kitchen sink instructions
          (registered-for-stacks [:integer :float :boolean :string :char :exec :print :code :parentheses :vector_boolean :vector_integer :vector_float :vector_string])
          
))

(defn random-thing
  "Returns a random thing"
  []
  ((rand-nth
     (list
      int-erc-10000
      float-erc-10000
      boolean-erc
      visible-char-erc
      string-erc
      vector-boolean-erc
      vector-string-erc
      vector-integer-erc
      vector-float-erc
      ))))

(defn push-random-things
  [push-state]
  (let [num-things (rand-nth (list (rand-int 10) (rand-int 500) (rand-int 500)))
        things (repeatedly num-things random-thing)]
    (reduce (fn [state thing]
              (let [literal-type (recognize-literal thing)]
                (cond
                  literal-type (push-item thing literal-type state)
                  (= [] thing) (push-item [] :vector_integer 
                                          (push-item [] :vector_float 
                                                     (push-item [] :vector_string 
                                                                (push-item [] :vector_boolean state)))))))
            push-state
            things)))

(defn error-function
  [individual]
  (let [start-state (->> (make-push-state)
                         (push-item (random-thing) :input)
                         (push-item (random-thing) :input)
                         (push-item "" :output)
                         (push-random-things))]
    (try
      (let [final-state (run-push (:program individual)
                                  start-state)]
        (assoc individual
           :errors '(2999 2999 2999)
           :final-state final-state))
      (catch Throwable e
        (println "\nCaught exception:" (.getMessage e))
        (println "---------------------------------------")
        (println "Program:")
        (prn (:program individual))
        (println "\n---------------------------------------")
        (println "Start state:")
        (prn start-state)
        (println "\n---------------------------------------")
        (throw e)
        ))))


; Define the argmap
(def argmap
  {:error-function error-function
   :atom-generators atom-generators
   :max-points 4000
   :max-genome-size-in-initial-program 2000
   :evalpush-limit 5000
   :epigenetic-markers [:close]
   })


; lein run clojush.problems.test.run-random-programs :random-individual-search true :max-program-executions 3000


;;;;;;;;;;;;; Below here are crashing programs to run and figure out what they do.

; Log 8

(reset! global-max-points 4000)
(reset! global-evalpush-limit 5000)


(def start-state-string-8 (read-string (slurp "log8.state")))

(def prog-8
  '(-2613.631309263882 integer_flush float_sin vector_string_take vector_float_swap boolean_not exec_rot (vector_integer_occurrencesof vector_integer_concat -7910 exec_do*times (exec_stackdepth float_tan vector_float_flush) code_empty vector_integer_occurrencesof vector_string_eq integer_shove char_yank vector_integer_dup_times vector_integer_empty "" vector_float_dup_times) (boolean_swap print_vector_integer -228.97209939727702 integer_shove exec_while (boolean_swap) float_inc integer_dup vector_boolean_swap code_container char_yankdup exec_pop (boolean_dup_items string_setchar code_extract code_list) vector_boolean_occurrencesof boolean_invert_first_then_and) (vector_string_replace exec_do*count (vector_boolean_rest print_boolean vector_float_emptyvector)) vector_boolean_yankdup string_fromchar vector_string_butlast float_yankdup boolean_or string_flush vector_integer_rest 36 vector_float_pop string_stackdepth vector_string_contains string_replacefirst boolean_dup_times vector_integer_replacefirst 103 exec_if (["vW+Hj,t1eA:f;kRhq LjIiWwK\\-k*~6Ma!.0YU@\\@tqh%`)5" "R" "qz'%GIt_c+koZuU6K-2_]Tv;Gq4$MyNd1P'" "qlr Xzh]bdnr({^$Z|%,%Syb4\",-K;4" "$0&DWcnR G^I>YR<W?W48]{" "z'2hP>XGp=_}^l.rCWx|SKb=>H!b<a1<M**Bim&4 *IOs" "wirT)'zB1?kl,0es" "\"6l:LY~[cj,kNt1bTBz#8kMrrl" "K})43\\L4{ao&7{:9/_wT" "^JZM;V9TKc bSlGAL8;hx" "g=[=l9.}$Mf#I%-dO?#`" "44h\"8k'G1 M(Fn$_bW\\*g;@Vn}3Qvz,`;bc" "xaPgdfL]H 8<Q,u/4MQkyV_\\Bo.-5>)g~UHnse\\$Ki9@)ES'`" "w|[iYGq0P*Ub" "2y,*JaQ_$juV/\"3p6]B[ofckEwU" "=^:@RG99GA" "RePt,1.BoEDEfiY])]G" "'({IN@Y~_5:lf7`q7" "m<:L4o>%p]>g:<O @9WjMT]U:*ShU:;Q4Zzp-3Nu#0;" "vAZ1wxE6W^;$\"y&" "vWg1e?<Ez(D\"b<F755J+" "f1c>c^HkodOzmZ7gL" "T>H]?zh-eTO9)S~" "=m-a9XY+JSKKT8,*pYx" "UP`za#E" "/=(cPKx" " 76GbDV++o_5:]93D7C7%pqg[" "rPxm'[S%+8" "YpQoSb+<+,:TEOt_0b ." "}`BP8,\"Z4)&-h4K" ".0\"0" "l9-?Qic$IINScR(TiLhG-z63l&QuiI2qu" "_\\Mrx%}7Uh7/}3,E{RP@Ruu\\qk,\"0nmesIia9fDI" "ppnp[N2sv.CgdN"]) (vector_float_conj float_yank vector_string_butlast) exec_if (vector_boolean_swap) (boolean_invert_second_then_and code_null) char_rot boolean_or char_rot vector_boolean_take vector_string_butlast integer_flush print_integer code_dup_items boolean_stackdepth vector_float_conj vector_boolean_nth vector_string_yank string_last [true true true] code_append code_position vector_string_contains code_dup vector_integer_dup_items vector_boolean_replace char_empty vector_float_pushall code_quote (vector_float_conj vector_float_eq vector_float_nth vector_boolean_swap) code_fromfloat string_emptystring integer_mod vector_float_eq boolean_rot code_atom string_dup_times string_take exec_yankdup float_gt exec_s () (vector_string_rest code_flush float_sin code_extract) (vector_float_conj) exec_noop vector_integer_stackdepth code_extract exec_y (boolean_yankdup exec_stackdepth code_fromboolean vector_float_rot code_dup_items exec_do*times (code_nth string_replacefirst code_wrap code_noop -2129.188757640274 vector_boolean_dup vector_float_occurrencesof) code_subst vector_boolean_take) float_flush -72 print_code vector_integer_last integer_eq exec_do*count () in2 vector_integer_swap float_swap string_take integer_fromfloat integer_yank vector_float_pop char_shove vector_float_indexof boolean_yank char_shove string_removechar float_dup_items char_eq print_code integer_fromfloat boolean_dup vector_string_length string_replacefirst code_empty integer_mod code_swap vector_integer_last vector_float_reverse boolean_yankdup exec_k (vector_string_yankdup exec_shove (float_frominteger boolean_and vector_integer_indexof char_shove vector_boolean_stackdepth) exec_shove () vector_float_first float_empty) (vector_integer_butlast code_swap) print_string code_fromfloat exec_eq float_sin \A vector_boolean_butlast vector_float_subvec vector_boolean_flush vector_boolean_eq vector_float_rest exec_string_iterate (in2) exec_pop () code_list integer_swap 122 exec_do*vector_string () string_last vector_boolean_yank string_dup vector_boolean_rest char_dup_times exec_do*vector_float (float_pop boolean_dup_times) code_atom exec_y (vector_boolean_take float_fromboolean code_stackdepth float_lt) code_cdr float_inc vector_integer_remove vector_boolean_conj float_lt char_fromfloat code_append float_dup_items -95 code_position integer_dup print_code vector_float_emptyvector exec_flush vector_string_eq exec_yankdup exec_do*times (integer_sub vector_float_indexof vector_integer_length) vector_boolean_pop vector_string_flush vector_boolean_stackdepth exec_do*while (string_fromchar integer_dec code_dup) integer_fromboolean code_pop print_float vector_integer_reverse vector_string_replace [false false false true true true true false true true true false false true false false true true true true true true false false true true true true true false true true false false false false false true true true true true true true false false true true true true false false false false true false true true true false false true true false true false true true false true false false false false false true false true true false true false false true true true false false true false true] string_dup tagged_210 char_isdigit char_isdigit vector_boolean_stackdepth code_extract vector_boolean_stackdepth vector_boolean_conj integer_dup_items float_sin string_pop float_eq vector_string_rest vector_float_concat string_replacefirstchar float_cos vector_boolean_contains float_eq vector_string_empty float_mult vector_boolean_pop float_dec vector_boolean_first in1 string_contains tagged_643 vector_integer_last code_dup_times code_fromfloat vector_boolean_occurrencesof vector_float_set vector_boolean_swap vector_float_first vector_float_take vector_integer_remove exec_dup () char_shove string_contains code_list 54 string_reverse vector_integer_dup_items exec_stackdepth tagged_657 exec_if () (vector_float_yank) vector_boolean_eq vector_float_last integer_mult code_subst exec_dup_times (boolean_fromfloat boolean_dup string_yankdup string_rest integer_max boolean_invert_first_then_and vector_string_flush print_string integer_lte vector_float_length vector_float_pushall boolean_xor string_conjchar) code_subst print_integer string_yankdup integer_lte integer_lte vector_boolean_dup_items char_swap vector_float_swap vector_boolean_eq integer_dup exec_s (float_gte integer_add) (vector_string_butlast vector_boolean_dup_items vector_integer_pushall vector_boolean_concat integer_mod vector_string_empty vector_string_conj float_fromstring) (float_rot exec_dup_items vector_integer_pop code_dup_times) string_last exec_dup_times () exec_do*vector_string () float_fromstring code_noop vector_integer_empty code_do code_cons integer_swap integer_fromchar exec_eq string_fromfloat code_container code_shove code_subst string_removechar exec_do*times (vector_string_set vector_integer_dup_items code_nth char_eq print_vector_string string_indexofchar code_stackdepth vector_string_replacefirst vector_integer_emptyvector vector_integer_indexof print_vector_integer vector_integer_flush) vector_integer_yankdup vector_integer_butlast boolean_not char_isletter float_fromstring print_float vector_float_eq float_stackdepth float_dup_items vector_integer_indexof vector_integer_empty boolean_pop vector_string_replace vector_integer_stackdepth char_flush boolean_empty vector_integer_butlast vector_integer_rest vector_boolean_stackdepth vector_integer_take integer_max vector_integer_rest code_fromfloat code_yank vector_boolean_dup_items vector_integer_butlast integer_fromboolean integer_mult vector_string_rot vector_float_conj integer_fromstring vector_boolean_occurrencesof char_dup_items exec_stackdepth vector_float_replacefirst vector_integer_rest exec_dup_items float_sub string_dup_items string_nth char_yank string_parse_to_chars vector_float_subvec [2045.314262046193 -6957.912025828715 7246.780363241727 -1367.9254081172003 8861.716526794327 6186.237995098949 3603.585467694362 -9547.1986353875 5463.362267589675] vector_boolean_length float_dup_items integer_empty vector_boolean_set string_dup_times code_null code_car vector_integer_set string_replacefirstchar float_mod vector_float_dup_times print_vector_float char_fromfloat exec_empty vector_float_eq vector_string_length vector_boolean_concat string_replacefirstchar vector_float_swap "a%dYA}B-@R*7_W1-g\\LH" code_atom vector_string_shove print_exec () integer_empty vector_string_length string_replacefirstchar exec_flush -7422.624099926993 float_dup_times boolean_invert_first_then_and code_pop vector_boolean_emptyvector vector_boolean_shove code_do*times vector_boolean_pop float_swap char_iswhitespace boolean_frominteger vector_float_rest vector_integer_first vector_string_replacefirst integer_rot char_frominteger vector_float_yankdup vector_boolean_rest boolean_yank vector_float_length string_flush -1684.1461258064064 vector_string_yankdup vector_integer_occurrencesof char_rot exec_y () vector_string_yank vector_string_butlast))

;; (run-push prog-8 start-state-string-8 true)

;; (count-points)

;; (defn list-it
;;   [times]
;;   (loop [thing true
;;          n times]
;;     (if (< n 0)
;;       thing
;;       (recur (list thing)
;;              (dec n)))))

;; (list-it 5000)
