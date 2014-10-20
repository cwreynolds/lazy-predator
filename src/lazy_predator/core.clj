(ns lazy-predator.core
  (:gen-class)
  (:require [lazy-predator.tree :as tree]
            [lazy-predator.fitness :as fitness]
            [lazy-predator.population :as population]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println args))


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
        terminals '(x y)]
    (clojure.pprint/pprint (make-population n d functions terminals size))))

