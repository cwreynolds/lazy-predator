(ns lazy-predator.core
  (:gen-class))

;; 2014-09-13 cwr: obviously very rough intial prototypes...

;;for testing: make GP programs from these functions
(def example-function-set
  '(+ - * /))

;; for testing: make GP programs with these terminals
(def example-terminal-set
  '(0 1 2 3 4 5 6 7 8 9 x y))

(defn gp-tree-size
  "measure the size of a GP tree, counts functions and terminals"
  [x]
  (count (flatten x)))

;; this might be the same function as make-individual,
;; or that might have additional bookkeeping
(defn build-gp-tree
  "make a random expression with given function names and terminal/leaf values"
  [functions terminals min-size max-size]
  (cond (< max-size 1) nil
        (< max-size min-size) nil
        (= max-size 1) (rand-nth terminals)
        :else (let [size0 (int (/ max-size 2))
                    tree0 (build-gp-tree functions terminals min-size size0)
                    size1 (- (dec max-size) (count-atoms tree0))
                    tree1 (build-gp-tree functions terminals min-size size1)]
                (cond (= nil tree0) tree1
                      (= nil tree1) tree0
                      :else (list (rand-nth functions)
                                  tree0
                                  tree1)))))

(defn make-individual [functions terminals min-size max-size]
  )

(defn make-deme [individuals]
  )

(defn make-population [demes]
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (println args))
