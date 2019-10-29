(ns search-algorithms.helper-functions
  (:use [clojure.data.priority-map]
        [search-algorithms.new-graph-generation]))

(defn in? [coll e]
  (some #(= e %) coll))

(defn format-edges [edges] (println (map #(str (clojure.string/join " " (seq %)) "\n") (keys edges))))

(defn debug [path] (println path) path)

(def edges-in-one-direction (generate-new-edges 26 5 10 (min-probability-threshold 26)))

(def edges-in-one-direction2 (generate-new-edges 26 5 10 (- (min-probability-threshold 26) 0.1)))

(defn reverse-path [[k v]] [(apply str (reverse k)) v])

(def paths (into edges-in-one-direction (map reverse-path edges-in-one-direction)))

(def paths2 (into edges-in-one-direction2 (map reverse-path edges-in-one-direction2)))

(def neighbors (apply merge-with concat (map (fn [[[from to] distance]] {(str from) [(str to)]}) paths)))

(def neighbours2 (apply merge-with concat (map (fn [[[from to] distance2]] {(str from) [(str to)]}) paths2)))

(def distance (comp paths str))

(def distance2 (comp paths2 str))

;(def edges-in-one-direction
;  {"ab" 2
;   "bc" 3
;   "cj" 1
;   "ad" 1
;   "dg" 2
;   "de" 1
;   "be" 2
;   "ef" 1
;   "cf" 1
;   "gh" 1
;   "hi" 2
;   "fi" 4
;   "ik" 2
;   "jk" 1})

;(def heuristic {"a" 3
;                "b"      3
;                "c"      2
;                "d"      3
;                "e"      2
;                "f"      2
;                "g"      3
;                "h"      2
;                "i"      1
;                "j"      1
;                "k"      0})