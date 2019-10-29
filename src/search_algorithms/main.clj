(ns search-algorithms.main
  (:use [clojure.data.priority-map]
  [search-algorithms.new-graph-generation]
  [search-algorithms.search-algorithms]
  [search-algorithms.helper-functions]))

(def heuristic (generate-heuristics "z" dijkstra distance neighbors))
(format-edges edges-in-one-direction)
(a* "a" "z" distance heuristic neighbors)
(dijkstra "a" "z" distance (constantly 0) neighbors)
(d* "a" "z" distance heuristic neighbors)
(get-open-nodes distance heuristic neighbors (priority-map ["a" ["a"] 0] (heuristic "a")) [])