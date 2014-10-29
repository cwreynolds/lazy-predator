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

;; (defn sin-sin-fitness
;;   ([program] (sin-sin-fitness program 100))
;;   ([program samples] (let [xs (repeatedly samples #(generators/float))
;;                            correct (map sin-sin-example xs)
;;                            evolved (map (fn [a]
;;                                           (binding [x a]
;;                                             (eval program)))
;;                                         xs)]
;;                        (/ (apply +
;;                                  (map fit/difference-squared correct evolved))
;;                           samples))))


;; (defn sin-sin-fitness
;;   ([program] (sin-sin-fitness program 100))
;;   ([program samples] (let [xs (repeatedly samples #(generators/float))
;;                            correct (map sin-sin-example xs)
;;                            evolved (map (fn [a]
;;                                           (binding [x a]
;;                                             (eval program)))
;;                                         xs)
;;                            sum-diff-sq (apply +
;;                                               (map fit/difference-squared
;;                                                    correct
;;                                                    evolved))]

;;                        (prn (list 'sum-diff-sq sum-diff-sq))
                       
;;                        (/ sum-diff-sq samples))))

;; xxx oh, I think this was returning an error metric, so higher fitness actually
;; meant worse fitness. Switching to simply inverting the sign. Bigger errors will be
;; more negative, hence lower fitness. Lazy Predator is supposed to only care about
;; relative fitness, so...

(defn sin-sin-fitness
  ([program] (sin-sin-fitness program 100))
  ([program samples] (let [xs (repeatedly samples #(generators/float))
                           correct (map sin-sin-example xs)
                           evolved (map (fn [a]
                                          (binding [x a]
                                            (eval program)))
                                        xs)
                           sum-diff-sq (apply +
                                              (map fit/difference-squared
                                                   correct
                                                   evolved))
                           ave-diff-sq-per-sample (- (/ sum-diff-sq samples))
                           fitness (if (Double/isNaN ave-diff-sq-per-sample)
                                     Double/NEGATIVE_INFINITY
                                     ave-diff-sq-per-sample)]

                       ;;(prn (list 'fitness fitness))
                       
                       fitness)))

(defn strawman-sin-sin-run
  "cobble together the first version of a run in Lazy Predator."
  [n population-count deme-count]
  (let [terminals '(x :float01)
        functions '{+   {:type :number :args (:number :number)}
                    -   {:type :number :args (:number :number)}
                    *   {:type :number :args (:number :number)}
                    /   {:type :number :args (:number :number)}
                    pow {:type :number :args (:number :number)}
                    sin {:type :number :args (:number)}
                    cos {:type :number :args (:number)}}] 
    (loop [individuals 0
           population (pop/make-population population-count
                                           deme-count
                                           functions
                                           terminals
                                           10)]
      (prn (list individuals
                 (pop/average-fitness population)))
      (when (< individuals n)
        (recur (inc individuals)
               (pop/next-gp-individual population
                                       sin-sin-fitness
                                       functions
                                       terminals))))))

;;; hmm, this 2014-10-27 run seems to have odd ups and downs in its ave fitness
;;; also seeing divides by zero and NaNs

;; with pop 8 2

;; lazy-predator.core> (strawman-sin-sin-run 50)
;; (0 0)
;; (1 0.5437949314242791)
;; (2 0.7096640908061159)
;; (3 0.8077045773907733)
;; (4 2.566118614247743)
;; (5 2.6779902238978908)
;; (6 2.1558764077202284)
;; (7 20.28671641426231)
;; (8 20.18074299210482)
;; (9 4.030758540062043)
;; (10 4.018699450036314)
;; (11 4.0465934982105205)
;; (12 4.037387987714548)
;; (13 4.055393643933965)
;; (14 4.026256509085159)
;; (15 4.044965665163207)
;; (16 4.033367013927862)
;; (17 4.131907692113581)
;; (18 4.136191853546186)
;; (19 22.853068734449376)
;; (20 22.860557328490902)
;; (21 22.89555886225056)
;; (22 23.269060254032105)
;; (23 23.65417740901982)
;; (24 23.793956664924405)
;; (25 23.873829300314803)
;; (26 23.85662814090676)
;; (27 1586.9336002820605)
;; (28 1587.001745054547)
;; (29 1587.7657438677936)
;; (30 72.40015138420843)
;; (31 72.3766549043301)
;; (32 89.78917997768525)
;; (33 91.11017379198549)
;; (34 13.93066966582762)
;; (35 12.81351287233871)
;; (36 12.744167509317112)
;; (37 79.35706004869743)
;; (38 97.64060435645874)
;; (39 97.72153942126018)
;; (40 1617.089921311504)
;; (41 1349.0452541772402)
;; (42 70.05924093533172)
;; (43 151.22462813059477)
;; (44 84.4877950401221)
;; (45 8.81265334488174)
;; (46 8.819545528000598)
;; (47 18.08663439919023)
;; (48 1.95140348002264E9)
;; (49 1.9514109817433434E9)
;; (50 1.9514109820969684E9)

;; with pop 8 1 -- works better?

;; lazy-predator.core> (strawman-sin-sin-run 50)
;; (0 0)
;; (1 0.49700409086854014)
;; (2 0.6402682809786566)
;; (3 3.4763036957383604)
;; (4 1.0800001159782469)
;; (5 10.260627097639478)
;; (6 8.563920463611655)
;; (7 6.519779237455602)
;; (8 6.771776581607156)
;; (9 6.804467801734266)
;; (10 57.60437256855857)
;; (11 13.46237241299151)
;; (12 137.14083005759215)
;; (13 170.73701587017416)
;; (14 168.86829160264654)
;; (15 596.6586735378851)
;; (16 564.9220840387703)
;; (17 667.1237175147816)
;; (18 663.0204321607239)
;; (19 669.3248726841022)
;; (20 734.9327644661273)
;; (21 734.0570038402419)
;; (22 508644.2319952725)
;; (23 593537.0495994963)
;; (24 593536.26503423)
;; (25 593526.2701995285)
;; (26 1081962.6929041597)
;; (27 1583237.4074609804)
;; (28 1583243.196368281)
;; (29 1846995.5749708768)
;; (30 2216316.3879788327)
;; (31 1848151.5578481976)
;; (32 2220697.7386781443)
;; (33 2467421.7844580314)
;; (34 2775409.386402902)
;; (35 3465726.575315357)
;; (36 2927163.931441555)
;; (37 2983201.528244166)
;; (38 2983080.2371673593)
;; (39 1.4385206726659077E8)
;; (40 1.1987721873217906E8)
;; (41 1.2159194211818892E8)
;; (42 1.4478157618625209E7)
;; (43 1.6306003894393066E12)
;; (44 1.9570631233499395E12)
;; (45 1.6310377467477385E12)
;; (46 5.983597281424013E12)
;; (47 9.067225508562354E12)
;; (48 1.1251564305060371E11)
;; (49 9.015863996740826E10)
;; (50 7.579962141092714E10)
;; nil
