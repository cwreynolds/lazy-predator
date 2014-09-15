(ns lazy-predator.core
  (:gen-class))

;; 2014-09-13 cwr: obviously very rough intial prototypes...

;;for testing: make GP programs from these functions
(def example-function-set
  '(+ - * /))

;; OK, but how do we declare the number of args for each function?
;; Later on, how to we associate types with each?
;; here is a prototype: a map of functions for a run, indexed by fn name
;;   nil is used here to represent no type (all types?)

;; (def example-function-set
;;   {'+ {:fn + :args '(:a nil :b nil)}
;;    '- {:fn - :args '(:a nil :b nil)}
;;    '* {:fn * :args '(:a nil :b nil)}
;;    '/ {:fn / :args '(:a nil :b nil)}})

;; (def example-function-set
;;   {'+ '(:a nil :b nil)
;;    '- '(:a nil :b nil)
;;    '* '(:a nil :b nil)
;;    '/ '(:a nil :b nil)})

;; oops, this is too simplified. for Strongly Typed GP we need a type
;; for each arg, but also for the return time of each function

(def example-function-set
  {'+ '(true true)
   '- '(true true)
   '* '(true true)
   '/ '(true true)
   'sin '(true)
   'cos '(true)})

;; for testing: make GP programs with these terminals
(def example-terminal-set
  '(0 1 2 3 4 5 6 7 8 9 x y))

(defn gp-tree-size
  "measure the size of a GP tree, counts functions and terminals"
  [x]
  (count (flatten x)))


;; (defn build-gp-tree
;;   "make a random expression with given function names and terminal/leaf values"
;;   [functions terminals size]
;;   (cond (< size 1) nil
;;         (= size 1) (rand-nth terminals)
;;         :else (let [size0 (int (/ size 2))
;;                     tree0 (build-gp-tree functions terminals size0)
;;                     size1 (- (dec size) (gp-tree-size tree0))
;;                     tree1 (build-gp-tree functions terminals size1)]
;;                 (cond (= nil tree0) tree1
;;                       (= nil tree1) tree0
;;                       :else (list (rand-nth (keys functions))
;;                                   ;;(rand-nth functions)
;;                                   tree0
;;                                   tree1)))))

;; (defn rand2
;;   "random number between two bounds"
;;   [low high]
;;   (+ low (rand (- high low))))

;; (defn build-gp-tree-arglist-subsize
;;   ""
;;   [size args]
;;   (let [fraction (/ 1.0 args)
;;         low (/ fraction 2)
;;         high (/ (+ fraction 1) 2)
;;         randomized-fraction (rand2 low high)
;;         randomized-fraction-of-size (* size randomized-fraction)]
;;     (int (+ randomized-fraction-of-size 0.5))))


(defn build-gp-tree-arglist
  "helper function for build-gp-tree"
  [arglist functions terminals size]
  (if (empty? arglist)
    nil
    (let [args (count arglist)
          ;; consider making this a random range around the 1/args value
          subsize (int (+ (/ size args) 0.5))
          ;; subsize (build-gp-tree-arglist-subsize size args)
          subtree (build-gp-tree functions terminals subsize)]
      (cons subtree
            (build-gp-tree-arglist (rest arglist)
                                   functions
                                   terminals
                                   (- size
                                      (gp-tree-size subtree)))))))

;; this might be the same function as make-individual,
;; or that might have additional bookkeeping, like a serial number
;;
;; originaly the args were [functions terminals min-size max-size]
;; I'm rethinking expessing size as a range
;; perhaps it should just be a single "size" or "target-size"
;; if we want initial populations to contain a range of sizes,
;;   that can be handled outside this function
;; changing args to [functions terminals size]
;;
(defn build-gp-tree
  "make a random expression with given function names, terminals and size"
  [functions terminals size]
  (cond (< size 1) nil
        (= size 1) (rand-nth terminals)
        :else (let [;; select random function from map
                    function (rand-nth (keys functions))
                    ;; index function map by selected function name
                    arglist (function functions)
                    ;; number of args for this function
                    arg-count (count arglist)
                    ;; list of subtrees for each arg
                    args (build-gp-tree-arglist arglist
                                             functions
                                             terminals
                                             (dec size))]
                ;; if any of those are nil...
                (if (some #(= % nil) args)
                  ;; return a non-nil one [XXX hope there is one!!!]
                  (first (remove #(= % nil) args))
                  ;; otherwise the fn consed onto arg expressions
                  (cons function args)))))

(defn print-gp-tree
  "for testing during development"
  [tree]
  (clojure.pprint/pprint tree)
  (print "size ")
  (print (gp-tree-size tree)))

(defn make-individual [functions terminals size]
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
