(ns search_algorithms.otherSearch
  (:use [clojure.data.priority-map]))

(declare a* get-open-nodes p)

(defn d*
  [start goal distance-fn heuristic-fn neighbours-fn]
  (let [next (a* start goal distance-fn heuristic-fn neighbours-fn)]
    (loop [path-to-check next
           complete-path []]
      (let [path-node (first path-to-check)
            open (get-open-nodes distance-fn heuristic-fn neighbours-fn (priority-map [path-node [path-node] 0] (heuristic-fn start)) [])
            [[open-node open-path open-distance] total] (first open)]
        (cond
          (empty? (rest path-to-check))
            (conj complete-path path-node)
          (= (first (rest path-to-check)) open-node)
            (recur (rest path-to-check) (conj complete-path path-node) )
          :else
            (recur (a* path-node goal distance-fn heuristic-fn neighbours-fn) complete-path)
          )))))

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
        (if (goal?-fn node)
          path
          (recur (conj closed node) (get-open-nodes distance-fn heuristic-fn neighbours-fn open closed)
                 ))))))

(defn in? [coll e]
  (some #(= e %) coll))

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

(defn dijkstra
  "Calculate shortest path with Dijkstra's algorithm.
  start is the first node.
  goal?-fn is called for each expanded node to check whether it is a goal
  distance-fn is called with from and to nodes and should return a numeric distance.
  neighbors-fn is called with the node and should return all the neighbors
  max-depth is the maximum depth of the path in steps."
  [start goal?-fn distance-fn neighbors-fn ]
  ;; Dijkstra is a special case of A* when the heuristic is zero
  (a* start goal?-fn distance-fn (constantly 0) neighbors-fn ))

(def edges-in-one-direction
  {"ab" 2
   "bc" 3
   "cj" 1
   "ad" 1
   "dg" 2
   "de" 1
   "be" 2
   "ef" 1
   "cf" 1
   "gh" 1
   "hi" 2
   "fi" 4
   "ik" 2
   "jk" 1})

(defn reverse-path [[k v]] [(apply str (reverse k)) v])
(def paths (into edges-in-one-direction (map reverse-path edges-in-one-direction)))


(def neighbors (apply merge-with concat
                      (map (fn [[[from to] distance]]
                             {(str from) [(str to)]})
                           paths)))

(defn goal= [n] (partial = n))
(def distance (comp paths str))

(def heuristic {"a" 3
                "b" 3
                "c" 2
                "d" 3
                "e" 2
                "f" 2
                "g" 3
                "h" 2
                "i" 1
                "j" 1
                "k" 0})

(defn debug [path s] (println s) path)

;(defn search-call [search start goal graph]
;  (search start goal distance heuristic neighbors))

(a* "a" "k" distance heuristic neighbors)
(dijkstra "a" "k" distance neighbors)
(d* "a" "k" distance heuristic neighbors)
(get-open-nodes  distance heuristic neighbors (priority-map ["a" ["a"] 0] (heuristic "a")) [])