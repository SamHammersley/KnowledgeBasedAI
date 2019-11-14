(ns search-algorithms.search-algorithms
  (:use [clojure.data.priority-map :exclude [subseq rsubseq]]
        [search-algorithms.helper-functions]
        [search-algorithms.new-graph-generation]))
(declare dijkstra)

(defn get-open-nodes [distance-fn heuristic-fn neighbours-fn open closed]
  (let [[[node path distance] total] (peek open)]
    (into (pop open)
          (for [neighbour (neighbours-fn node)
                :let [new-node neighbour
                      new-path (conj path neighbour)
                      new-distance (+ distance (distance-fn node neighbour))]
                :when (not (in? closed new-node))]
            [[new-node new-path new-distance] (+ new-distance (heuristic-fn neighbour))]))))

(defn a*
  "Calculate shortest path with A*.
  Note the heuristic function must be monotonous, increasing and never overestimate
  (i.e. it must be admissible) to guarantee an optimal result.
  start is the first node.
  goal is either a predicate or a goal node which is checked against.
  distance-fn is called with from and to nodes and should return a numeric distance.
  heuristic-fn is called with a node and should return a numeric estimated distance to the nearest goal.
  neighbors-fn is called with the node and should return all the neighbors.
  This function returns the most optimal path (vector of nodes) and the total distance of that path"
  [start goal distance-fn heuristic-fn neighbours-fn]
  (let [goal?-fn (if (fn? goal) goal #(= goal %))]
    (loop [closed [] open (priority-map [start [start] 0] (heuristic-fn start))]
      (let [[[node path distance] total] (first open)]
        (cond
          (goal?-fn node)
          [path total]
          :else
          (recur (conj closed node) (get-open-nodes distance-fn heuristic-fn neighbours-fn open closed)))))))

(defn dijkstra
  "Calculate shortest path with Dijkstra's algorithm.
  start is the first node.
  goal?-fn is called for each expanded node to check whether it is a goal
  distance-fn is called with from and to nodes and should return a numeric distance.
  neighbors-fn is called with the node and should return all the neighbors
  max-depth is the maximum depth of the path in steps."
  [start goal distance-fn heuristic-fn neighbors-fn]
  ;; Dijkstra is a special case of A* when the heuristic is zero
  (a* start goal distance-fn (constantly 0) neighbors-fn))

(def test-edges-in-one-direction2 {
                                   "02" 2
                                   "04" 4
                                   "05" 5
                                   "14" 1
                                   "15" 3
                                   "23" 5
                                   "24" 6
                                   "45" 2
                                   "34" 1
                                   })
(def test-paths2 (into test-edges-in-one-direction2 (map reverse-path test-edges-in-one-direction2)))
(def test-neighbors2 (apply merge-with concat (map (fn [[[from to] test-distance2]] {(str from) [(str to)]}) test-paths2)))
(def test-distance2 (comp test-paths2 str))
(def test-heuristic2 (generate-heuristics "3" dijkstra test-distance2 test-neighbors2))

(defn d*
  [start goal distance-fn heuristic-fn neighbours-fn]
  (let [next (first (a* start goal distance-fn heuristic-fn neighbours-fn))]
    (loop [path-to-check next
           closed []
           ;newGraph detectChange
           ]
      (let [path-node (first path-to-check)
            distance-fn test-distance2
            heuristic-fn test-heuristic2
            neighbours-fn test-neighbors2
            open (get-open-nodes distance-fn heuristic-fn neighbours-fn (priority-map [path-node [path-node] 0] (heuristic-fn path-node)) closed)
            open-node (first (first (peek open)))]
        ;(println 'path= path-to-check 'open= open 'open-node-heuristic= (heuristic-fn open-node) 'next-node-heuristic= (heuristic-fn (first (rest path-to-check))))
        (cond
          (empty? (rest path-to-check))
            (conj closed path-node)
          (= (first (rest path-to-check)) open-node)
            (recur (rest path-to-check) (conj closed path-node))
          :else
            (recur (first (a* path-node goal distance-fn heuristic-fn neighbours-fn)) closed))))))


(defn detectChanges [graph] (;get sensor data
                              ))