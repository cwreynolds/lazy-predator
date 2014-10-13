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


(defn remove-nth [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

;; call this routine once per new individual, then likelihood scales with the population

(defn migrate
  "given a population, move an individual from one deme to another.
   A population is a vector of demes, a deme is a vector of individuals"
  [population]
  (let [shuffled-deme-indices (generators/shuffle (range (count population)))
        a (first shuffled-deme-indices)
        b (second shuffled-deme-indices)
        deme-a (nth population a)
        deme-b (nth population b)
        random-a (generators/uniform 0 (count deme-a))
        individual (nth deme-a random-a)
        new-deme-a (remove-nth deme-a random-a)
        new-deme-b (conj deme-b individual)]
    (assoc (assoc population a new-deme-a) b new-deme-b)))

;; (migrate [[1 1 1 1] [2 2 2] [3 3] [4 4 4 4 4]])


;; (migrate
;; [[1 1 1 1] [2 2 2] [3 3] [4 4 4 4 4]])  =>
;; [[1 1 1 1] [2 2 2] [3] [4 4 4 4 4 3]]
;; [[1 1 1] [2 2 2] [3 3] [4 4 4 4 4 1]]
;; [[1 1 1 1 2] [2 2] [3 3] [4 4 4 4 4]]
;; [[1 1 1 1] [2 2 2 3] [3] [4 4 4 4 4]]
;; [[1 1 1 1 4] [2 2 2] [3 3] [4 4 4 4]]
;; [[1 1 1 1] [2 2] [3 3] [4 4 4 4 4 2]]
;; [[1 1 1 1] [2 2] [3 3 2] [4 4 4 4 4]]
;; [[1 1 1 1] [2 2] [3 3 2] [4 4 4 4 4]]
;; [[1 1 1] [2 2 2] [3 3] [4 4 4 4 4 1]]
;; [[1 1 1 1] [2 2 2 4] [3 3] [4 4 4 4]]
;; [[1 1 1 1] [2 2 2] [3 3 4] [4 4 4 4]]
;; [[1 1 1 1] [2 2 2] [3 3 4] [4 4 4 4]]
;; [[1 1 1] [2 2 2 1] [3 3] [4 4 4 4 4]]

(defn maybe-migrate
  "given a population, move an individual from one deme to another.
   A population is a vector of demes, a deme is a vector of individuals"
  [population]
  (when (tree/maybe? 0.005) ;; 1/200
    (migrate population)))


;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

