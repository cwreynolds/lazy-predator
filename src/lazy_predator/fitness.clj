(ns lazy-predator.fitness
  (:gen-class)
  (:require [clojure.data.generators :as generators]))


;; 20141018 just a for instance fitness function for now:
;;
;; GP test function:  sin(x/0.03 * ((0.09 * sin(x / 0.05)) + 0.11))
;; to see a plot of the function use this:
;; https://www.google.com/search?q=sin(x%2F0.03+*+((0.09+*+sin(x+%2F+0.05))+%2B+0.11))+x+is+from+-0.1+to+1.1

(defn sin-sin-example [x]
  (Math/sin (* (/ x 0.03)
               (+ (* (Math/sin (/ x 0.05))
                     0.09)
                  0.11))))



;; note sure if this is a reasonable approach:

(def ^:dynamic x) ;; where this corresponds to one to the GP terminals

(defn difference-squared [a b]
  (let [d (- a b)]
    (* d d)))

(defn sin-sin-fitness
  ([program] (sin-sin-fitness program 100))
  ([program samples] (let [xs (repeatedly samples #(generators/float))
                           correct (map sin-sin-example xs)
                           evolved (map (fn [a]
                                          (binding [x a]
                                            (eval program)))
                                        xs)]
                       (/ (apply +
                                 (map difference-squared correct evolved))
                          samples))))

;; > (time (sin-sin-fitness '(* x x) 1000))
;; "Elapsed time: 403.581 msecs"
;; 0.5723415233753604
;; > (time (sin-sin-fitness '(* x x) 1000))
;; "Elapsed time: 409.202 msecs"
;; 0.5420461385214744
;; > (time (sin-sin-fitness '(* x x) 1000))
;; "Elapsed time: 453.547 msecs"
;; 0.5519993160828698
;; > (time (sin-sin-fitness '(* x x) 1000))
;; "Elapsed time: 466.243 msecs"
;; 0.5659170776715866

;; > (time (sin-sin-fitness '(* x x) 100))
;; "Elapsed time: 49.881 msecs"
;; 0.5156795733290083
;; > (time (sin-sin-fitness '(* x x) 100))
;; "Elapsed time: 52.712 msecs"
;; 0.5079030815616534
;; > (time (sin-sin-fitness '(* x x) 100))
;; "Elapsed time: 51.463 msecs"
;; 0.4278853680719341
;; > (time (sin-sin-fitness '(* x x) 100))
;; "Elapsed time: 56.66 msecs"
;; 0.4909356386973297
