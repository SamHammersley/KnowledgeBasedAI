(ns search-algorithms.main
  (:use [search-algorithms.helper-functions]
        [search-algorithms.search-algorithms]
        [search-algorithms.new-graph-generation]))

(def edges-in-one-direction (generate-new-edges 10 1 60 (min-probability-threshold 10)))
(def paths (into edges-in-one-direction (map reverse-path edges-in-one-direction)))
(def neighbors (apply merge-with concat (map (fn [[[from to] distance]] {(str from) [(str to)]}) paths)))
(def distance (comp paths str))
(def heuristic (generate-heuristics "h" dijkstra distance neighbors))

(print-formatted-edges edges-in-one-direction)
;(dijkstra "a" "h" distance (constantly 0) neighbors)
;(a* "a" "h" distance heuristic neighbors)
(d* "a" "h" distance heuristic neighbors)