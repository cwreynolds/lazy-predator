(ns lazy-predator.tree
  (:gen-class)
  (:require [clojure.data.generators :as generators]
             [clojure.pprint :as pp]))


(def mark-4-function-set-example
  '{+ {:type :number :args (:number :number)}
    - {:type :number :args (:number :number)}
    * {:type :number :args (:number :number)}
    / {:type :number :args (:number :number)}
    pow {:type :number :args (:number :number)}
    sin {:type :number :args (:number)}
    cos {:type :number :args (:number)}})

(defn get-function-type
  "given a GP function set, and a function name, get its type"
  [function-symbol functions]
  (assert (contains? functions function-symbol)
          (str function-symbol " not in function set"))
  (:type (function-symbol functions)))

(defn get-function-arglist
  "given a GP function set, and a function name, get its arglist"
  [function-symbol functions]
  (assert (contains? functions function-symbol)
          (str function-symbol " not in function set"))
  (:args (function-symbol functions)))


;; this may be just for early development
;; or maybe still check once per fun?

(defn is-mark-4-function-set?
  "verify that an object has format compatible with the 'mark 4 function set'"
  [fs]
  (assert (map? fs)) ;; a map...
  (assert (> (count fs) 0)) ;; of non-zero length
  (assert (every? symbol? (keys fs))) ;; whose keys are all symbols
  (assert (every? (fn [m] (and (map? m) ;; and whose vals are all maps with
                              (contains? m :type) ;; at least these two keys
                              (contains? m :args)))
                  (vals fs))
          "some function's map did not contain :type and :args"))

;; (get-function-type '+ mark-4-function-set-example)
;; (get-function-arglist '+ mark-4-function-set-example)
;; (is-mark-4-function-set? functions)

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

;; for testing: make GP programs with these terminals
;; (def example-terminal-set
;;   '(0 1 2 3 4 5 6 7 8 9 x y))

;; these need to be marked with types

;; testing ephemeral random constants
(def example-terminal-set
  '(x y :float01 :float-plus-minus-1 :float-plus-minus-10))

(defn choose-terminal
  "choose a random terminal from set, instantiate ephemeral constants"
  [terminals]
  (let [selected (generators/rand-nth terminals)]
    (cond (= selected :float01) (generators/float)
          (= selected :float-plus-minus-1) (dec (* 2 (generators/float)))
          (= selected :float-plus-minus-10) (- (* 20 (generators/float)) 10)
          :else selected)))

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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
  (is-mark-4-function-set? functions)
  (cond (< size 1) nil
        ;; (= size 1) (generators/rand-nth terminals)
        (= size 1) (choose-terminal terminals)
        :else (let [;; select random function from map 
                    function-name (generators/rand-nth (keys functions)) 
                    ;; arglist for selected function
                    arglist (get-function-arglist function-name functions) 
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
                  (cons function-name args)))))

(defn print-gp-tree
  "for testing during development"
  [tree]
  (clojure.pprint/pprint tree)
  (print "size ")
  (print (gp-tree-size tree)))

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

;; XXX maybe temporary, just use this CL throwback as a utility for now
(defn maplist
  ([s] (maplist identity s))
  ([f s] (when-let [s (seq s)] (lazy-seq (cons (f s) (maplist f (next s)))))))

;; (maplist (fn [x] [(first x) x]) '(a b c d))
;; => ([a (a b c d)] [b (b c d)] [c (c d)] [d (d)])

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

;; 2014-10-04

;; given A and B:
;;   find receptors in A and choose one at random, call it R
;;   find donors in B and choose one at random, call it D
;;   copy A, until R is encountered, replace it with D


(defn- linearize-gp-tree-descriptor
  "packages the description of a gp subexpression into a map"
  [subexpression parent type]
  {:subtree subexpression
   :parent parent
   :type type})

;; XXX FIX -- not a good idea to have an argument named type since it
;; shadows the Clojure built-in function that gets an object's type. 

;; (defn- linearize-gp-tree-2
;;   "given a gp tree (and its parent cons, and its STGP type),
;;    append a description of each subexpression to the given table (a vector)"
;;   [tree functions terminals parent type table]
;;   (let [new-table (concat table [(linearize-gp-tree-descriptor tree parent type)])]

;; ;;     (if-not (list? tree)

;;     (if (= 'clojure.lang.Cons (class tree))
      
;;       ;; new-table
;;       (do
;;         (prn (list "linearize-gp-tree-2 done: (class tree)=" (class tree) " tree=" tree))
;;         new-table)
      
;;       (do
;;         ;; not sure about these error checks:
;;         ;; just a temporary dev expedient, or leave in long term?
;;         (assert (not (empty? tree))
;;                 "GP tree is unexpectedly empty")
;;         (assert (contains? functions (first tree))
;;                 "first of expression not in function set?")
;;         (apply concat
;;                new-table
;;                (maplist (fn [arglist]
;;                           (linearize-gp-tree-2 (first arglist) functions terminals arglist type []))
;;                         (rest tree)))))))


(defn- linearize-gp-tree-2
  "given a gp tree (and its parent cons, and its STGP type),
   append a description of each subexpression to the given table (a vector)"
  [tree functions terminals parent tree-type table]
  (let [new-table (concat table [(linearize-gp-tree-descriptor tree parent tree-type)])]

    ;; used to say (if-not (list? tree) ...) not sure why that stopped
    ;; working while debugging sin-sin example
    
    (if  (or (keyword? tree)
             (symbol? tree)
             (number? tree))
      
      new-table
      
      ;; (do
      ;;   (prn (list "linearize-gp-tree-2 done: (type tree)=" (type tree) " (list? tree)=" (list? tree) " tree=" tree))
      ;;   new-table)
      
      (do
        ;; not sure about these error checks:
        ;; just a temporary dev expedient, or leave in long term?
        (assert (not (empty? tree))
                "GP tree is unexpectedly empty")
        (assert (contains? functions (first tree))
                "first of expression not in function set?")
        (apply concat
               new-table
               (maplist (fn [arglist]
                          (linearize-gp-tree-2 (first arglist) functions terminals arglist tree-type []))
                        (rest tree)))))))


(defn linearize-gp-tree
  "convert tree into table of all subexpressions, their parent cones, and STGP types"
  [tree functions terminals]
  (is-mark-4-function-set? functions)
  (vec (linearize-gp-tree-2 tree functions terminals :root :any [])))


(defn test-linearize-gp-tree
  []
  (let [sample-tree '(+ a
                        (* (- b c)
                           (/ d
                              (! e))))
        functions '{+ {:type :number :args (:number :number)}
                    - {:type :number :args (:number :number)}
                    * {:type :number :args (:number :number)}
                    / {:type :number :args (:number :number)}
                    ! {:type :number :args (:number)}}
        terminals '(a b c d e)]
    (do (clojure.pprint/pprint sample-tree)
        (clojure.pprint/pprint (linearize-gp-tree sample-tree functions terminals)))))



;; (test-linearize-gp-tree)
;; =>
;; (+ a (* (- b c) (/ d (! e))))
;; [{:subtree (+ a (* (- b c) (/ d (! e)))), :parent :root, :type :any}
;;  {:subtree a, :parent (a (* (- b c) (/ d (! e)))), :type :any}
;;  {:subtree (* (- b c) (/ d (! e))), :parent ((* (- b c) (/ d (! e)))),:type :any}
;;  {:subtree (- b c), :parent ((- b c) (/ d (! e))), :type :any}
;;  {:subtree b, :parent (b c), :type :any}
;;  {:subtree c, :parent (c), :type :any}
;;  {:subtree (/ d (! e)), :parent ((/ d (! e))), :type :any}
;;  {:subtree d, :parent (d (! e)), :type :any}
;;  {:subtree (! e), :parent ((! e)), :type :any}
;;  {:subtree e, :parent (e), :type :any}]
;; nil


;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

;; 2014-10-06
;; crossover experiment

(defn- gp-crossover-splice
  ""
  [tree-a parent-a subtree-a subtree-b functions terminals]
  (if (identical? parent-a
                  (:parent subtree-a))
    (:subtree subtree-b)
    (if (list? tree-a)
      (cons (first tree-a)
            (maplist (fn [arglist] (gp-crossover-splice (first arglist) arglist subtree-a subtree-b functions terminals))
                     (rest tree-a)))
      tree-a)))


(defn gp-crossover
  "given two GP trees, replace a random subtree of A with a random subtree of B"
  [tree-a tree-b functions terminals]
  (is-mark-4-function-set? functions)
  (assert (not (empty? functions)))
  (assert (not (empty? terminals)))
  (let [table-a (linearize-gp-tree tree-a functions terminals)
        table-b (linearize-gp-tree tree-b functions terminals)
        subtree-a (generators/rand-nth table-a)
        subtree-b (generators/rand-nth table-b)]

    (newline)
    (pp/pprint 'table-a) (pp/pprint table-a) (newline)
    (pp/pprint 'table-b) (pp/pprint table-b) (newline)
    (pp/pprint 'subtree-a) (pp/pprint subtree-a) (newline)
    (pp/pprint 'subtree-b) (pp/pprint subtree-b) (newline)
    
    (gp-crossover-splice tree-a :root subtree-a subtree-b functions terminals)))


(defn test-gp-crossover-splice []
  (let [tree-a '(a (b 1 1)
                   (a 0 1))
        tree-b '(x (y 8 9)
                   (w 9))
        ;; keep both versions:
        subtree-a (linearize-gp-tree-descriptor (second tree-a) (rest tree-a) :foo)
        subtree-b (linearize-gp-tree-descriptor tree-b :root :foo)
        ;;subtree-a (linearize-gp-tree-descriptor (nth tree-a 2) (rest (rest tree-a)) :foo)
        ;;subtree-b (linearize-gp-tree-descriptor (second tree-b) (rest tree-b) :foo)
        functions '{a {:type :foo :args (:foo :foo)} 
                    b {:type :foo :args (:foo :foo)}
                    c {:type :foo :args (:foo :foo)}
                    x {:type :foo :args (:foo :foo)}
                    y {:type :foo :args (:foo :foo)}
                    w {:type :foo :args (:foo)}}
        terminals '(0 1 8 9)]
    (let [spliced (gp-crossover-splice tree-a :root subtree-a subtree-b functions terminals)]
      (prn (gp-tree-size spliced))
      spliced)))

;; (test-gp-crossover-splice)  => 
;; (a (x (y 8 9)
;;       (w 9))
;;    (a 0 1))


;; (test-gp-crossover-splice)
;; (tree-a (a (b 1 1) (a 0 1)) parent-a :root false (:subtree subtree-a) (b 1 1) (:parent subtree-a) ((b 1 1) (a 0 1)))
;; (tree-a (b 1 1) parent-a ((b 1 1) (a 0 1)) true (:subtree subtree-a) (b 1 1) (:parent subtree-a) ((b 1 1) (a 0 1)))
;; (tree-a (a 0 1) parent-a ((a 0 1)) false (:subtree subtree-a) (b 1 1) (:parent subtree-a) ((b 1 1) (a 0 1)))
;; (tree-a 0 parent-a (0 1) false (:subtree subtree-a) (b 1 1) (:parent subtree-a) ((b 1 1) (a 0 1)))
;; (tree-a 1 parent-a (1) false (:subtree subtree-a) (b 1 1) (:parent subtree-a) ((b 1 1) (a 0 1)))
;; 10
;; (a (x (y 8 9) (w 9)) (a 0 1))

;; second test:

;; (test-gp-crossover-splice)
;; (tree-a (a (b 1 1) (a 0 1)) parent-a :root false (:subtree subtree-a) (a 0 1) (:parent subtree-a) ((a 0 1)))
;; (tree-a (b 1 1) parent-a ((b 1 1) (a 0 1)) false (:subtree subtree-a) (a 0 1) (:parent subtree-a) ((a 0 1)))
;; (tree-a (a 0 1) parent-a ((a 0 1)) true (:subtree subtree-a) (a 0 1) (:parent subtree-a) ((a 0 1)))
;; (tree-a 1 parent-a (1 1) false (:subtree subtree-a) (a 0 1) (:parent subtree-a) ((a 0 1)))
;; (tree-a 1 parent-a (1) false (:subtree subtree-a) (a 0 1) (:parent subtree-a) ((a 0 1)))
;; 7
;; (a (b 1 1) (y 8 9))


;; random thought: the spec of functions and terminals form a type of grammer



(defn test-gp-crossover [n]
  (let [tree-a '(aaa (bbb d
                          (aaa d e f))
                     (ccc e)
                     (bbb (bbb d e)
                          f))
        tree-b '(XXX (YYY U V W)
                     (ZZZ (XXX U)
                          (ZZZ V W)))
        functions '{aaa {:type :foo :args (:foo :foo :foo)}
                    bbb {:type :foo :args (:foo :foo)}
                    ccc {:type :foo :args (:foo)}
                    XXX {:type :foo :args (:foo)}
                    YYY {:type :foo :args (:foo :foo :foo)}
                    ZZZ {:type :foo :args (:foo :foo)}}
        terminals '(d e f U V W)]
    (doseq [i (range n)]
      ;;(clojure.pprint/pprint (gp-crossover tree-a tree-b functions terminals))
      (prn (gp-crossover tree-a tree-b functions terminals)))))

;; (test-gp-crossover 100)  =>
;; (aaa (bbb d (aaa d (YYY U V W) f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e (YYY U V W))) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (XXX U) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (YYY U V W) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb W f))
;; (aaa (bbb V (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa (ZZZ (XXX U) (ZZZ V W)) e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa V (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa W e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W)))))
;; (aaa (bbb d (aaa d e f)) (ccc U) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e (XXX U))) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d (YYY U V W) f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) W (bbb (bbb d e) f))
;; (aaa (bbb (XXX U) (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d (XXX U)) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d U) f))
;; (aaa (bbb (XXX U) (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa (YYY U V W) e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc V) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) W (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) (XXX U)))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb V f))
;; (aaa (bbb d (aaa d U f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) (ZZZ (XXX U) (ZZZ V W))))
;; (aaa (bbb d (aaa d e V)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc W) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa U e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d V) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d U f)) (ccc e) (bbb (bbb d e) f))
;; (aaa W (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb U (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W)))) (ccc e) (bbb (bbb d e) f))
;; (aaa (YYY U V W) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc V) (bbb (bbb d e) f))
;; W
;; (aaa (bbb d (aaa d e f)) (ccc (YYY U V W)) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W))) f))
;; (aaa (bbb d U) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d V) (ccc e) (bbb (bbb d e) f))
;; (aaa (ZZZ V W) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e W)) (ccc e) (bbb (bbb d e) f))
;; (ZZZ (XXX U) (ZZZ V W))
;; (XXX U)
;; (aaa (bbb d (aaa V e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb U e) f))
;; (aaa (bbb d (aaa d e f)) W (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W))))
;; (aaa (bbb d U) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc V) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa (ZZZ V W) e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e U)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e U)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d W) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb V (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa W e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc U) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb V e) f))
;; (aaa (XXX U) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) W))
;; (aaa (bbb (XXX U) (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb U e) f))
;; (aaa (bbb d (aaa d e f)) (ccc (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W)))) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc V) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) U)
;; (aaa (bbb d (aaa d e f)) U (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc V) (bbb (bbb d e) f))
;; W
;; (aaa (bbb (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W))) (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W)))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) V))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d (YYY U V W)) f))
;; (aaa (bbb W (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb (ZZZ V W) (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb (XXX U) e) f))
;; (aaa (bbb W (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d (ZZZ (XXX U) (ZZZ V W)) f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) W))
;; (aaa (YYY U V W) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb V e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) W))
;; (aaa (bbb d (aaa d e f)) (ZZZ V W) (bbb (bbb d e) f))
;; (XXX U)
;; (aaa (bbb d (aaa d e f)) W (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) W))
;; (aaa (bbb d (aaa d e f)) (ccc e) V)
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) (ZZZ V W)))
;; (aaa V (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d V) f))
;; (aaa (bbb d (aaa d W f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W))) (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W))) e f)) (ccc e) (bbb (bbb d e) f))
;; (XXX (YYY U V W) (ZZZ (XXX U) (ZZZ V W)))
;; W
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb W f))
;; (aaa (bbb (ZZZ V W) (aaa d e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa W e f)) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d U) f))
;; (aaa (bbb d (ZZZ (XXX U) (ZZZ V W))) (ccc e) (bbb (bbb d e) f))
;; (aaa (bbb d (aaa d e f)) (ccc e) (bbb (bbb d e) W))
;; nil

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 


(defn maybe?
  "returns true with a probability of 'likelihood',
    on average (maybe? 1/3) will return true once every 3 calls."
  [likelihood]
  (assert (<= 0 likelihood 1) "likelihood should be between 0 and 1")
  (> likelihood
     (generators/float)))

;; (count (filter #{true} (take 100000 (repeatedly #(maybe? 0.25)))))

(defn clip-number
  "given a number force it to be within a given interval, by default the interval [0 1]"
  ([n] (clip-number n 0 1))
  ([n min max] (cond (> n max) max
           (< n min) min
           :else n)))

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

;; 2014-10-08
;; jiggle mutation experiment
;; occasionally replace a number with slightly displaced value
;; this ought to respect STGP types, it is just a simple prototype


(defn jiggle-float01 [n]
  (clip-number (let [max (+ n 0.01)
                     min (- n 0.01)]
                 (+ min
                    (* (- max min)
                       (generators/float))))))


(defn jiggle-gp-tree [tree]
  (if (list? tree)
    (cons (first tree)
          (map jiggle-gp-tree
               (rest tree)))
    (if (and (number? tree)
             (maybe? 0.1))
      (jiggle-float01 tree)
      tree)))

(defn test-jiggle-gp-tree [n]
  (let [tree '(a 1
                 (b (d 2 3)
                    (e 4 5)))]
    (doseq [i (range n)]
      (prn (jiggle-gp-tree tree)))))

(defn test-jiggle-float01
  []
  (loop [n 0
         i 0]
    (prn n)
    (when (< i 1000)
      
      (recur (jiggle-float01 n)
             (inc i)))))

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

;; 2014-10-09
;; hoist mutation experiment
;;
;; occasionally replace a tree with one of its own subtrees

(defn hoist-gp-subtree [tree functions terminals]
  "given a GP tree, select a random subtree to replace the whole tree"
    (:subtree (generators/rand-nth (linearize-gp-tree tree functions terminals))))

(defn test-hoist-gp-subtree [n]
  (let [tree '(aaa (bbb d
                        (aaa d e f))
                   (ccc e)
                   (bbb (bbb d e)
                        f))
        functions {'aaa '(:foo :foo :foo)
                   'bbb '(:foo :foo)
                   'ccc '(:foo)}
        terminals '(d e f)]
    (doseq [i (range n)]
      (prn (hoist-gp-subtree tree functions terminals)))))

;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 


;;; XXX TESTING STUFF
;;; should be in separate test file


(defmacro with-repeatable-random-numbers [& body]
  "bind the random number generator to a fixed value"
  ;; 2147483647 is the 8th Mersenne prime, M31: (2^31)-1
  `(binding [generators/*rnd* (java.util.Random. 2147483647)]
     ~@body))


(defn test-build-gp-tree []
  (with-repeatable-random-numbers
    (print-gp-tree (build-gp-tree '{+   {:type :number :args (:number :number)}
                                    -   {:type :number :args (:number :number)}
                                    *   {:type :number :args (:number :number)}
                                    /   {:type :number :args (:number :number)}
                                    pow {:type :number :args (:number :number)}
                                    sin {:type :number :args (:number)}
                                    cos {:type :number :args (:number)}}
                                  '(x y :float01 :float-plus-minus-1 :float-plus-minus-10)
                                  30))))

;; as of 2014-10-19:
;;
;; (tree/test-build-gp-tree)  =>
;;
;; (pow
;;  (sin
;;   (sin
;;    (+
;;     (/ (sin (sin y)) (* x 4.237693548202515))
;;     (/ (* y -7.695364952087402) -7.452949285507202))))
;;  (*
;;   (/ (+ x (/ 0.8364409804344177 7.372931241989136)) x)
;;   (pow (- y (cos -3.808445930480957)) 0.9838296175003052)))
;; size 30
;; nil



;; -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 


;; BTW, I'd like to tweak the pp params to print like this:
;;
;;     (sin (sin (* (* (/ (cos (- y 0.7829017639160156))
;;                        (cos x))
;;                     (- (cos (sin x))
;;                        (- y y)))
;;                  (+ (/ x
;;                        (+ -0.36693501472473145 y))
;;                     (sin (- (cos -0.038851022720336914)
;;                             (cos y)))))))

;; (looks like there is a good toolkit in clojure.pprint for building custom pretty
;; printers: http://clojuredocs.org/clojure.pprint )



;; (print-gp-tree (build-gp-tree example-function-set example-terminal-set 30))



:eof
