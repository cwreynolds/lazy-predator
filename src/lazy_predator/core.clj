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

;; not sure how to handle this namespace issue, define these wrappers for now:
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn pow [x y] (Math/pow x y))
;; (defn / [x y]
;;   (prn (list '/ x y))
;;   (if (= y 0) 0 (clojure.core// x y)))
(defn / [x y]
  ;;(prn (list '/ x y))
  (if (or (= y 0)
          (= y 0.0))
    0
    (clojure.core// x y)))

;; 20141018 just a for-instance fitness function for now:
;;
;; GP test function:  sin(x/0.03 * ((0.09 * sin(x / 0.05)) + 0.11))
;; to see a plot of the function use this:
;; https://www.google.com/search?q=sin(x%2F0.03+*+((0.09+*+sin(x+%2F+0.05))+%2B+0.11))+x+is+from+-0.1+to+1.1

(defn sin-sin-example [x]
  (Math/sin (* (/ x 0.03)
               (+ (* (Math/sin (/ x 0.05))
                     0.09)
                  0.11))))



;; not sure if this is a reasonable approach:

(def ^:dynamic x) ;; where this corresponds to one of the GP terminals

;; XXX FIX remember this is now the "old" curve

(defn- square [x] (* x x))

(defn sin-sin-fitness
  ([program] (sin-sin-fitness program 100))
  ([program samples] (let [;; just for test, place samples regularly, not random
                           xs (repeatedly samples #(generators/float))
                           ;;xs (map #(/ % (float samples)) (range (inc samples)))

                           correct (map sin-sin-example xs)
                           evolved (map (fn [a]
                                          (binding [x a]
                                            (eval program)))
                                        xs)
                           
                           ;; sum-diff-sq (apply +
                           ;;                    (map fit/difference-squared
                           ;;                         correct
                           ;;                         evolved))
                           ;; ave-diff-sq-per-sample (- (/ sum-diff-sq samples))
                           ;; fitness (if (Double/isNaN ave-diff-sq-per-sample)
                           ;;           Double/NEGATIVE_INFINITY
                           ;;           ave-diff-sq-per-sample)
                           
                           ;; sum-diff-sq (apply +
                           ;;                    (map fit/absolute-difference
                           ;;                         correct
                           ;;                         evolved))
                           ;; ave-diff-sq-per-sample (- (/ sum-diff-sq samples))
                           ;; fitness (if (Double/isNaN ave-diff-sq-per-sample)
                           ;;           Double/NEGATIVE_INFINITY
                           ;;           ave-diff-sq-per-sample)

                           ;; fitness (- (apply max
                           ;;                   (map fit/absolute-difference
                           ;;                        correct
                           ;;                        evolved)))
                           
                           fitness (- (square (apply +
                                                     (map fit/absolute-difference
                                                          correct
                                                          evolved))))

                           ]
                       fitness)))

(defn test-sin-sin-fitness
  ""
  []
  (let [body-of-sin-sin-example '(Math/sin (* (/ x 0.03)
                                              (+ (* (Math/sin (/ x 0.05))
                                                    0.09)
                                                 0.11)))]
    (sin-sin-fitness body-of-sin-sin-example)))

(defn strawman-sin-sin-run
  "cobble together the first version of a run in Lazy Predator."
  [n population-count deme-count]
  (let [terminals '(x :float01)
        functions '{+   {:type :number :args (:number :number)}
                    -   {:type :number :args (:number :number)}
                    *   {:type :number :args (:number :number)}
                    /   {:type :number :args (:number :number)}
                    ;; pow {:type :number :args (:number :number)}
                    sin {:type :number :args (:number)}
                    cos {:type :number :args (:number)}}] 
    (loop [individuals 0
           population (pop/make-population population-count
                                           deme-count
                                           functions
                                           terminals
                                           10)]
      ;; (prn (list individuals
      ;;            (pop/average-fitness population)))


      (when (= 0 (mod individuals 20))
        (newline)
        ;; (prn (list 'demes (map count population)))
        (prn individuals)
        (doseq [x (pop/population-snapshot population)] 
          (pp/pprint x)))


      
      (when (< individuals n)
        (recur (inc individuals)
               (pop/next-gp-individual population
                                       sin-sin-fitness
                                       functions
                                       terminals))))))

'(strawman-sin-sin-run 80000 200 10)

'lazy-predator.core
