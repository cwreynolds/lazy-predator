(ns lazy-predator.core
  (:gen-class)
  (:require [lazy-predator.tree :as tree]
            [lazy-predator.fitness :as fit]
            [lazy-predator.population :as pop]
            [clojure.data.generators :as generators]
            [clojure.pprint :as pp]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println args))

(defn next-gp-individual
  ""
  [population fitness functions terminals]
  (let [shuffled-demes (shuffle population)
        deme (first shuffled-demes)
        other-demes (vec (rest shuffled-demes))
        shuffled-individuals (shuffle deme)
        _ (assert (> (count shuffled-individuals) 3)) ; ???
        [a b c] shuffled-individuals
        other-individuals (vec (drop 3 shuffled-individuals))


        a (assoc a :fitness (generators/float))
        b (assoc b :fitness (generators/float))
        c (assoc c :fitness (generators/float))
        
        ;; a (assoc a :fitness (fitness (:tree a)))
        ;; b (assoc b :fitness (fitness (:tree b)) )
        ;; c (assoc c :fitness (fitness (:tree c)) )

        sorted-by-fitness (reverse (sort-by :fitness (list a b c)))
        [p q] sorted-by-fitness

        ;; _ (prn (list '(:tree p) (:tree p)))
        ;; _ (prn (list '(:tree q) (:tree q)))
        ;; _ (prn (list '(tree/gp-crossover (:tree p) (:tree q) functions terminals)
        ;;              (tree/gp-crossover (:tree p) (:tree q) functions terminals)))
        
        o (pop/make-individual (tree/gp-crossover (:tree p)
                                                  (:tree q)
                                                  functions
                                                  terminals))]

    ;; (pp/pprint (list a b c))

    (newline)
    (prn :p)
    (pp/pprint p)
    (prn :q)
    (pp/pprint q)
    (prn :o)
    (pp/pprint o)
    
    ;; test fitness of a b and c
    ;; rank fitness, call the best two p and q
    ;;
    ;; crossover p q, call resulting offspring o
    ;;
    ;; conjoin two parents and new offspring to the end of the selected deme,
    ;; conjoin that deme to the end of the other deems, return this new population
    (conj other-demes
          (conj (conj (conj other-individuals
                            p)
                      q)
                o))))

;;-----------------------------------------------------------------------------------

'(
  (strawman-sin-sin-run) =>

  ;; XXX aha! -- note that these linearized trees both have only one subtree

  table-a
  [{:subtree
    (cos
     (+
      (pow (sin 0.7366800904273987) 0.7415241003036499)
      (- 0.03777742385864258 0.028750479221343994))),
    :parent :root,
    :type :any}]

  table-b
  [{:subtree
    (+ (/ (+ 0.6755239963531494 x) x) (* (sin 0.9771628975868225) x)),
    :parent :root,
    :type :any}]

  subtree-a
  {:subtree
   (cos
    (+
     (pow (sin 0.7366800904273987) 0.7415241003036499)
     (- 0.03777742385864258 0.028750479221343994))),
   :parent :root,
   :type :any}

  subtree-b
  {:subtree
   (+ (/ (+ 0.6755239963531494 x) x) (* (sin 0.9771628975868225) x)),
   :parent :root,
   :type :any}
  )


;;-----------------------------------------------------------------------------------



;; not sure haow to handle this namespace issue, define these wrappers for now:

(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn pow [x y] (Math/pow x y))



(defn strawman-sin-sin-run
  "cobble together the first version of a run in Lazy Predator."
  []
  (let [functions '{+   {:type :number :args (:number :number)}
                    -   {:type :number :args (:number :number)}
                    *   {:type :number :args (:number :number)}
                    /   {:type :number :args (:number :number)}
                    pow {:type :number :args (:number :number)}
                    sin {:type :number :args (:number)}
                    cos {:type :number :args (:number)}}
        terminals '(x :float01)
        population (pop/make-population 8 2 functions terminals 10)]
    ;;(pp/pprint population)
    (next-gp-individual population fit/sin-sin-fitness functions terminals)
    ))


;; temp, just for debugging
(defn test-sin-sin-crossover [n]
  (let [tree-a '(pow (+ (cos 0.919853925704956) (/ x x)) (+ x (sin 0.573377251625061)))
        tree-b '(* (sin (+ x 0.5772578716278076)) (sin (cos (sin (cos x)))))

        functions '{+   {:type :number :args (:number :number)}
                    -   {:type :number :args (:number :number)}
                    *   {:type :number :args (:number :number)}
                    /   {:type :number :args (:number :number)}
                    pow {:type :number :args (:number :number)}
                    sin {:type :number :args (:number)}
                    cos {:type :number :args (:number)}}
        terminals '(x :float01)]
    (doseq [i (range n)]
      (prn (tree/gp-crossover tree-a tree-b functions terminals)))))
