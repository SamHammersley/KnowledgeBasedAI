(ns search-algorithms.main
  (:use [search-algorithms.helper-functions]
        [search-algorithms.search-algorithms]
        [search-algorithms.new-graph-generation]))

(def edges-in-one-direction (generate-new-edges 10 1 60 (min-probability-threshold 10)))
(def paths (into edges-in-one-direction (map reverse-path edges-in-one-direction)))
(def neighbors (apply merge-with concat (map (fn [[[from to] distance]] {(str from) [(str to)]}) paths)))
(def distance (comp paths str))
(def heuristic (generate-heuristics "d" dijkstra distance neighbors))

(def test-edges-in-one-direction {
                                  "02" 2
                                  "04" 4
                                  "05" 5
                                  "14" 1
                                  "15" 3
                                  "23" 5
                                  "24" 6
                                  "45" 2
                                  })

(def test-paths (into test-edges-in-one-direction (map reverse-path test-edges-in-one-direction)))
(def test-neighbors (apply merge-with concat (map (fn [[[from to] test-distance]] {(str from) [(str to)]}) test-paths)))
(def test-distance (comp test-paths str))
(def test-heuristic (generate-heuristics "3" dijkstra test-distance test-neighbors))

;Show the graph in use
(print-formatted-edges edges-in-one-direction)

;Completeness tests
(dijkstra "a" "d" distance (constantly 0) neighbors)
(a* "a" "d" distance heuristic neighbors)
(d* "a" "d" distance heuristic neighbors)

;Optimality Tests
(= ["5" "0" "2" "3"] (first(a* "5" "3" test-distance test-heuristic test-neighbors)))
(= ["5" "0" "2" "3"] (d* "5" "3" test-distance test-heuristic test-neighbors))
(= ["5" "0" "2" "3"] (first (dijkstra "5" "3" test-distance (constantly 0) test-neighbors)))


;Allows JVM warm-up
(print-time(dorun (for [x (range 0 50000)] (dijkstra "a" "d" distance (constantly 0) neighbors))))
(print-time(dorun (for [x (range 0 50000)] (a* "a" "d" distance heuristic neighbors))))
(print-time(dorun (for [x (range 0 50000)] (d* "a" "d" distance heuristic neighbors))))

;Time complexity tests
(print-time (time (dorun (for [x (range 0 1000)] (a* "a" "d" distance heuristic neighbors)))))
(print-time (time (dorun (for [x (range 0 1000)] (d* "a" "d" distance heuristic neighbors)))))
(print-time (time (dorun (for [x (range 0 1000)] (dijkstra "a" "d" distance (constantly 0) neighbors)))))