(ns lazy-predator.tree
  (:gen-class))


;; 2014-09-13 cwr: obviously very rough intial prototypes...

;;for testing: make GP programs from these functions
;; (def example-function-set
;;   '(+ - * /))

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
;; (def example-terminal-set
;;   '(0 1 2 3 4 5 6 7 8 9 x y))

;; testing ephemeral random constants
(def example-terminal-set
  '(x y :float01 :float-plus-minus-1 :float-plus-minus-10))

(defn choose-terminal
  "choose a random terminal from set, instantiate ephemeral constants"
  [terminals]
  (let [selected (rand-nth terminals)]
    (cond (= selected :float01) (rand)
          (= selected :float-plus-minus-1) (dec (* 2 (rand)))
          (= selected :float-plus-minus-10) (- (* 20 (rand)) 10)
          :else selected)))

(defn gp-tree-size
  "measure the size of a GP tree, counts functions and terminals"
  [x]
  (count (flatten x)))


(declare build-gp-tree)

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
        ;; (= size 1) (rand-nth terminals)
        (= size 1) (choose-terminal terminals)
        :else (let [ ;; select random function from map
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


;; (print-gp-tree (build-gp-tree example-function-set example-terminal-set 30))



;; playing with crossover

;; (defn find-all-subtrees (tree)
;;   (let [subtrees []
;;         r (fn r [x]
;;                   (if (seq? x) 
;;                     (conj (r (first x))
;;                           (r (rest x)))
;;                     x))]
;;     ((r tree))))

;; (defn find-all-subtrees [tree]
;;   (if (seq? tree) 
;;     (conj (find-all-subtrees (first tree))
;;           (find-all-subtrees (rest tree)))
;;     [tree]))

;; (defn find-all-subtrees [tree]
;;   (if (coll? tree)
;;     (if (empty? tree)
;;       []
;;       (conj (find-all-subtrees (first tree))
;;             (find-all-subtrees (rest tree))))
;;     [tree]))

;; ;; try going back to this, currently broken
;; (defn find-all-subtrees [tree]
;;   (let [subtrees []
;;         r (fn r [x]
;;             (if (coll? x)
;;               (when (not (empty? x))
;;                 (conj subtrees (first x))
;;                 (find-all-subtrees (first x))
;;                 (find-all-subtrees (rest x))) 
;;               (conj subtrees x)))]
;;     (r tree)
;;     subtrees))

;; ;; try going back to this, currently broken
;; (defn find-all-subtrees [tree]
;;   (let [ ;;subtrees []
;;         r (fn r [tree subtrees]
;;             (if (coll? tree)
;;               (when (not (empty? tree))
                
;;                 ;; (conj subtrees (first tree))
;;                 ;; (r (first tree))
;;                 ;; (r (rest tree))

                
;;                 ;; (r (rest tree)
;;                 ;;    (conj subtrees (first tree)))

;;                 (vec (concat subtrees
;;                              (r (first tree) [])
;;                              (r (rest tree) [])))

;;                 ;; (vec (concat (conj subtrees (first tree))
;;                 ;;              (r (first tree) [])
;;                 ;;              (r (rest tree) [])))

                

;;                 ) 
;;               (conj subtrees tree)))]
;;     (r tree [])))


;; ;; try going back to this, currently broken
;; (defn find-all-subtrees [tree]
;;   (let [r (fn r [tree subtrees]
;;             (if (coll? tree)
;;               (when (not (empty? tree))
;;                 (vec (concat (conj subtrees tree)
;;                              (r (first tree) [])
;;                              (r (rest tree) []))))))]
;;     (r tree [])))


;; try going back to this, currently broken
(defn find-all-subtrees [tree]
  (let [r (fn r [tree subtrees]
            (when (and (coll? tree)
                       (not (empty? tree)))
              (vec (concat (conj subtrees tree)
                             (r (first tree) [])
                             (r (rest tree) [])))))]
    (r tree [])))


(def sample-tree
  '(+ a
      (* (- b c)
         (/ d
            (! e)))))

;;; (doseq [x (find-all-subtrees sample-tree)] (clojure.pprint/pprint x))


;; (+ a (* (- b c) (/ d (! e))))
;; a
;; (* (- b c) (/ d (! e)))
;; (- b c)
;; b
;; c
;; (/ d (! e))
;; d
;; (! e)
;; e

