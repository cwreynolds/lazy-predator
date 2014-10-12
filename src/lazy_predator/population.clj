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

(defn make-individual [functions terminals size]
  {:tree (tree/build-gp-tree functions terminals size)
   :id (next-serial-number-for-individual)})

(defn make-deme [individuals]
  )

(defn make-population [demes]
  )
