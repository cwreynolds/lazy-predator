(ns lazy-predator.population
  (:gen-class)
  (:require [lazy-predator.tree :as tree]))

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
