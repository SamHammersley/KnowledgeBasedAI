(ns search-algorithms.search-algorithms
  (:use [search-algorithms.helper-functions]
        [search-algorithms.new-graph-generation]
        [clojure.data.priority-map]))

(declare heuristic2)

(defn get-open-nodes [distance-fn heuristic-fn neighbours-fn open closed]
  (let [[[node path distance] total] (first open)]
    (into (pop open)
          (for [neighbour (neighbours-fn node)
                :let [new-node neighbour
                      new-path (conj path neighbour)
                      new-distance (+ distance (distance-fn node neighbour))]
                :when (not (in? closed new-node))]
            [[new-node new-path new-distance] (+ new-distance (heuristic-fn neighbour))])))
  )


(defn a*
  "Calculate shortest path with A*.
  Note the heuristic function must be monotonous, increasing and never overestimate
  (i.e. it must be admissible) to guarantee an optimal result.
  start is the first node.
  goal?-fn is called for each expanded node to check whether it is a goal
  distance-fn is called with from and to nodes and should return a numeric distance.
  heuristic-fn is called with a node and should return a numeric estimated distance to the nearest goal.
  neighbors-fn is called with the node and should return all the neighbors"
  [start goal distance-fn heuristic-fn neighbours-fn ]
  (let [goal?-fn (if (fn? goal) goal #(= goal %))]
    (loop [closed [] open (priority-map [start [start] 0] (heuristic-fn start))]
      (let [[[node path distance] total] (first open)]
        (cond
          ;(= path ["a" "b" "c"])
          ;  (let [
          ; distance-fn distance2
          ;      heuristic-fn heuristic2
          ;      neighbours-fn neighbours2
          ;]
          ;(recur (conj closed node) (get-open-nodes distance-fn heuristic-fn neighbours-fn open closed)
          ;))
          (goal?-fn node)
            [path total]
          :else
            (recur (conj closed node) (get-open-nodes distance-fn heuristic-fn neighbours-fn open closed))
          )))))


(defn d*
  [start goal distance-fn heuristic-fn neighbours-fn]
  (let [next (first (a* start goal distance-fn heuristic-fn neighbours-fn))]
    (println next)
    (loop [path-to-check next
           complete-path []]
      (let [path-node (first path-to-check)
            ;distance-fn distance2
            ;heuristic-fn heuristic2
            ;neighbours-fn neighbours2
            open (get-open-nodes distance-fn heuristic-fn neighbours-fn (priority-map [path-node [path-node] 0] (heuristic-fn start)) complete-path)
            [[open-node open-path open-distance] total] (first open)]
        (cond
          (empty? (debug (rest path-to-check)))
            (conj complete-path path-node)
          (= (debug (first (rest path-to-check))) (debug open-node))
            (recur (rest path-to-check) (conj complete-path path-node))
          :else
            (recur (first (a* path-node goal distance-fn heuristic-fn neighbours-fn)) complete-path)
          )))))

(defn dijkstra
  "Calculate shortest path with Dijkstra's algorithm.
  start is the first node.
  goal?-fn is called for each expanded node to check whether it is a goal
  distance-fn is called with from and to nodes and should return a numeric distance.
  neighbors-fn is called with the node and should return all the neighbors
  max-depth is the maximum depth of the path in steps."
  [start goal distance-fn heuristic-fn neighbors-fn ]
  ;; Dijkstra is a special case of A* when the heuristic is zero
  (a* start goal distance-fn (constantly 0) neighbors-fn ))

;(def heuristic2 (generate-heuristics "d" dijkstra distance2 neighbours2))
