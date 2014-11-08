(ns lazy-predator.population
  (:gen-class)
  (:require [lazy-predator.tree :as tree]
            [clojure.data.generators :as generators]))

;; 2014-10-11

;; we need some sort of abstraction to hold the parameters and state of a run
;;     for one example, the population should be a property of a run
;;     similarly, an individual should have a serial number,
;;         and the last one used should be stored in the run
;;         (although a date-timestamp, or a GUID are other approaches)

;; temporary global state:
(def *serial-number* (atom 0))

(defn next-serial-number-for-individual []
  (swap! *serial-number* inc))

(defn make-individual
  "make a new individual to add to a population, from a tree,
   or from parameters to create a random tree"
  ([tree]
     {:tree tree
      :id (next-serial-number-for-individual)})
  ([functions terminals size]
     (make-individual (tree/build-gp-tree functions terminals size))))

(defn make-deme [n functions terminals size]
  "create a deme of N random individuals with given parameters"
  (vec (repeatedly n #(make-individual functions terminals size))))

(defn make-population [n d functions terminals size]
  "create a population of N random individuals (with given parameters) in D demes"
  (vec (repeatedly d #(make-deme (/ n d) functions terminals size))))

(defn test-make-population [n d size]
  (let [functions {'a '(:foo)
                   'b '(:foo :foo)
                   'c '(:foo :foo :foo)}
        terminals '(x y)]
    (clojure.pprint/pprint (make-population n d functions terminals size))))

;; (test-make-population 6 2 10)  =>
;; [[{:tree (a (c (a x) (c x y (a x)) y)), :id 7}
;;   {:tree (c (c x y x) (b x (a x)) y), :id 8}
;;   {:tree (b (c x (b y y) y) (b x (a y))), :id 9}]
;;  [{:tree (a (a (b (c y (a y) y) y))), :id 10}
;;   {:tree (a (c (c x y (b x y)) x x)), :id 11}
;;   {:tree (c (b x (a x)) (b x (a x)) y), :id 12}]]
;; nil

;; need utilities to adjust populations to deal with too many or too few

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

;; 2014-10-12
;;
;; super-preliminary migration model
;;
;; was originally thinking about a "ring of islands" model but decided to try a fully
;; connected model. Pick two demes, remove a random individual from A, add it to B
;;
;; currently called "migrate" and "maybe-migrate" -- might be too short/generic?


;; (defn remove-nth [coll index]
;;   (vec (concat (subvec coll 0 index)
;;                (subvec coll (inc index)))))

;; (defn migrate
;;   "given a population, move an individual from one deme to another.
;;    A population is a vector of demes, a deme is a vector of individuals"
;;   [population]
;;   (if (< (count population) 2)
;;     population
;;     (let [shuffled-deme-indices (generators/shuffle (range (count population)))
;;           a (first shuffled-deme-indices)
;;           b (second shuffled-deme-indices)
;;           deme-a (nth population a)
;;           deme-b (nth population b)
;;           random-a (generators/uniform 0 (count deme-a))
;;           individual (nth deme-a random-a)
;;           new-deme-a (remove-nth deme-a random-a)
;;           new-deme-b (conj deme-b individual)]
;;       (assoc (assoc population a new-deme-a) b new-deme-b))))

;; ;; (migrate
;; ;; [[1 1 1 1] [2 2 2] [3 3] [4 4 4 4 4]])  =>
;; ;; [[1 1 1 1] [2 2 2] [3] [4 4 4 4 4 3]]
;; ;; [[1 1 1] [2 2 2] [3 3] [4 4 4 4 4 1]]
;; ;; [[1 1 1 1 2] [2 2] [3 3] [4 4 4 4 4]]
;; ;; [[1 1 1 1] [2 2 2 3] [3] [4 4 4 4 4]]
;; ;; [[1 1 1 1 4] [2 2 2] [3 3] [4 4 4 4]]
;; ;; [[1 1 1 1] [2 2] [3 3] [4 4 4 4 4 2]]
;; ;; [[1 1 1 1] [2 2] [3 3 2] [4 4 4 4 4]]
;; ;; [[1 1 1 1] [2 2] [3 3 2] [4 4 4 4 4]]
;; ;; [[1 1 1] [2 2 2] [3 3] [4 4 4 4 4 1]]
;; ;; [[1 1 1 1] [2 2 2 4] [3 3] [4 4 4 4]]
;; ;; [[1 1 1 1] [2 2 2] [3 3 4] [4 4 4 4]]
;; ;; [[1 1 1 1] [2 2 2] [3 3 4] [4 4 4 4]]
;; ;; [[1 1 1] [2 2 2 1] [3 3] [4 4 4 4 4]]

(defn migrate
  "given a population, pick 2 demes at random, pick and individual at random from both,
   swap them. A population is a vector of demes, a deme is a vector of individuals"
  [population]
  (if (< (count population) 2)
    population
    (let [shuffled-demes (shuffle population)
          [deme1 deme2] shuffled-demes
          other-demes (vec (drop 2 shuffled-demes))
          shuffled-individuals1 (shuffle deme1)
          shuffled-individuals2 (shuffle deme2)
          new-deme1 (vec (conj (drop 1 shuffled-individuals1)
                               (first shuffled-individuals2)))
          new-deme2 (vec (conj (drop 1 shuffled-individuals2)
                               (first shuffled-individuals1)))] 
      (conj (conj other-demes new-deme1) new-deme2))))

;; (pop/migrate
;; [[1 1 1 1] [2 2 2] [3 3] [4 4 4 4 4]])
;; [[2 2 2] [3 3] [1 4 4 4 4] [4 1 1 1]]
;; [[3 3] [1 1 1 1] [4 2 2] [2 4 4 4 4]]
;; [[2 2 2] [1 1 1 1] [4 3] [3 4 4 4 4]]
;; [[1 1 1 1] [2 2 2] [4 3] [3 4 4 4 4]]
;; [[3 3] [2 2 2] [4 1 1 1] [1 4 4 4 4]]
;; [[1 1 1 1] [2 2 2] [3 4 4 4 4] [4 3]]
;; [[3 3] [4 4 4 4 4] [1 2 2] [2 1 1 1]]


;; call this routine once per new individual, then likelihood scales with the population
;; (allow probability value to be passed in?)

(defn maybe-migrate
  "occasionally move an individual from one population deme to another."
  [population]
  (if (tree/maybe? 0.02) ;; 1/50
    (migrate population)
    population))


;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

(defn average
  ""
  [x]
  (if (empty? x)
    0
    (/ (reduce + x)
       (count x))))

;; maybe have this collect other stats like max and min?

(defn average-fitness
  "average fitness of a population"
  [population]
  (average (remove (fn [x] (or (nil? x)
                              (Double/isNaN x)))
                   (map :fitness 
                        (flatten population)))))

(defn population-snapshot
  "returns min/average/max fitness of a population, plus the max fit tree"
  [population]
  (let [flat-population (flatten population)
        selected-individuals (remove (fn [x] (let [f (:fitness x)]
                                              (or (nil? f)
                                                  (Double/isNaN f))))
                                     flat-population)
        selected-fitnesses (map :fitness selected-individuals)
        ave (average selected-fitnesses)
        min (when-not (empty? selected-individuals)
              (apply min-key :fitness selected-individuals))
        max (when-not (empty? selected-individuals)
              (apply max-key :fitness selected-individuals))]

    ;; (clojure.pprint/pprint population)
    ;; (clojure.pprint/pprint min)
    ;; (clojure.pprint/pprint max)
    
    (list (:fitness  min)
          ave
          (:fitness max)
          (:tree max))))

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

;; (defn next-gp-individual
;;   "creates new population with one new individual"
;;   [population fitness functions terminals]
;;   (let [shuffled-demes (shuffle (maybe-migrate population))
;;         deme (first shuffled-demes)
;;         other-demes (vec (rest shuffled-demes))
;;         _ (assert (> (count deme) 3))   ; ???
;;         shuffled-individuals (shuffle deme)
;;         [a b c] shuffled-individuals
;;         other-individuals (vec (drop 3 shuffled-individuals))

;;         af (assoc a :fitness (fitness (:tree a)))
;;         bf (assoc b :fitness (fitness (:tree b)) )
;;         cf (assoc c :fitness (fitness (:tree c)) )

;;         sorted-by-fitness (reverse (sort-by :fitness (list af bf cf)))
;;         [p q] sorted-by-fitness

;;         tree (tree/gp-crossover (:tree p)
;;                                 (:tree q)
;;                                 functions
;;                                 terminals)
        
;;         ;; parameters for mutations should be passed in as args
;;         ;; 20141030 hoist likelihood: 0.05 -> 0.2
;;         ;; 0.2 -> 0.4 which seems ridiculously high
;;         ;; back to 0.2
;;         ;; set to conservative "occasional" value along with hard limit
;;         tree (if (tree/maybe? 1/20)
;;                (tree/hoist-gp-subtree tree functions terminals)
;;                tree)

;;         ;; parameters for mutations should be passed in as args
;;         ;; 20141104 decided to try smaller trees, change 60 to 30
;;         tree (tree/limit-gp-tree-size 60 tree functions terminals)
;;         ;; tree (tree/limit-gp-tree-size 30 tree functions terminals)
        
;;         tree (tree/jiggle-gp-tree tree)
;;         o (make-individual tree)
;;         ]
    
;;     ;; conjoin two parents and new offspring to the end of the selected deme,
;;     ;; conjoin that deme to the end of the other deems, return this new population
;;     (conj other-demes
;;           (conj (conj (conj other-individuals
;;                             p)
;;                       q)
;;                 o))))


;; 20141107

(defn next-gp-individual
  "creates new population with one new individual"
  [population fitness functions terminals]
  (let [shuffled-demes (shuffle (maybe-migrate population))
        deme (first shuffled-demes)
        other-demes (vec (rest shuffled-demes))
        _ (assert (> (count deme) 3))   ; ???
        shuffled-individuals (shuffle deme)
        [a b c] shuffled-individuals
        other-individuals (vec (drop 3 shuffled-individuals))

        ;; 20141107
        ;; af (assoc a :fitness (fitness (:tree a)))
        ;; bf (assoc b :fitness (fitness (:tree b)))
        ;; cf (assoc c :fitness (fitness (:tree c)))
        ;; 20141107
        ;; Ah OK, here is the problem we've passed in a fitness
        ;; function but we are using it to measure chohort fitness.
        ;; Maybe this function take a cohort-fitness or a tree-fitness
        ;; function, defaulting appropriately.
        ;; [af bf cf] (fitness (map :tree [a b c]))
        ;; this implementation seems awkward:
        three-fitnesses (fitness (map :tree [a b c]))
        af (assoc a :fitness (nth three-fitnesses 0))
        bf (assoc b :fitness (nth three-fitnesses 1))
        cf (assoc c :fitness (nth three-fitnesses 2))

        
        sorted-by-fitness (reverse (sort-by :fitness (list af bf cf)))
        [p q] sorted-by-fitness

        tree (tree/gp-crossover (:tree p)
                                (:tree q)
                                functions
                                terminals)
        
        ;; parameters for mutations should be passed in as args
        ;; 20141030 hoist likelihood: 0.05 -> 0.2
        ;; 0.2 -> 0.4 which seems ridiculously high
        ;; back to 0.2
        ;; set to conservative "occasional" value along with hard limit
        tree (if (tree/maybe? 1/20)
               (tree/hoist-gp-subtree tree functions terminals)
               tree)

        ;; parameters for mutations should be passed in as args
        ;; 20141104 decided to try smaller trees, change 60 to 30
        tree (tree/limit-gp-tree-size 60 tree functions terminals)
        ;; tree (tree/limit-gp-tree-size 30 tree functions terminals)
        
        tree (tree/jiggle-gp-tree tree)
        o (make-individual tree)
        ]
    
    ;; conjoin two parents and new offspring to the end of the selected deme,
    ;; conjoin that deme to the end of the other deems, return this new population
    (conj other-demes
          (conj (conj (conj other-individuals
                            p)
                      q)
                o))))
