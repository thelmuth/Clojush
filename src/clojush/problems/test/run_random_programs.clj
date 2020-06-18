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

(defn vector-vector-boolean-erc
  []
  (let [length (rand-int 100)]
    (vec (repeatedly length vector-boolean-erc))))

(defn vector-vector-integer-erc
  []
  (let [length (rand-int 100)]
    (vec (repeatedly length vector-integer-erc))))

(defn vector-vector-float-erc
  []
  (let [length (rand-int 100)]
    (vec (repeatedly length vector-float-erc))))

(defn vector-vector-string-erc
  []
  (let [length (rand-int 100)]
    (vec (repeatedly length vector-string-erc))))

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
           vector-vector-boolean-erc
           vector-vector-integer-erc
           vector-vector-float-erc
           vector-vector-string-erc
           )
          ;; Kitchen sink instructions
          (registered-for-stacks [:integer :float :boolean :string :char :exec :print :code :parentheses :vector_boolean :vector_integer :vector_float :vector_string :vector_vector_integer :vector_vector_float :vector_vector_boolean :vector_vector_string])

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
      vector-vector-boolean-erc
      vector-vector-integer-erc
      vector-vector-float-erc
      vector-vector-string-erc
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
                                                                (push-item [] :vector_boolean state
                                                                           (push-item [[]] :vector_vector_integer state
                                                                                (push-item [[]] :vector_vector_float state
                                                                                    (push-item [[]] :vector_vector_string state
                                                                                        (push-item [[]] :vector_vector_boolean state)))))))))))
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
