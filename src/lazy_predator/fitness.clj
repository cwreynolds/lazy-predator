(ns lazy-predator.fitness
  (:gen-class)
  (:require [clojure.data.generators :as generators]))


(defn difference-squared [a b]
  (let [d (- a b)]
    (* d d)))

(defn absolute-difference [a b]
  (Math/abs (- a b)))
