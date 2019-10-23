(ns search_algorithms.otherSearch
  (:use [clojure.data.priority-map]))

(declare a*)

(defn d*
  [start goal?-fn distance-fn heuristic-fn neighbors-fn max-depth]
  (loop [next (a* start goal?-fn distance-fn heuristic-fn neighbors-fn max-depth)]
    [open (priority-map [start [start] 0] (heuristic-fn start))
  depth 0]
    (cond
      (nil? (rest next))
        (a* start goal?-fn distance-fn heuristic-fn neighbors-fn max-depth)
      (= (first (rest next)) (pop open))
        (recur (rest next))
      :else
        (recur (a* ((first next) goal?-fn distance-fn heuristic-fn neighbors-fn max-depth)))
      )))

(defn a*
  "Calculate shortest path with A*.
  Note the heuristic function must be monotonous, increasing and never overestimate
  (i.e. it must be admissible) to guarantee an optimal result.
  start is the first node.
  goal?-fn is called for each expanded node to check whether it is a goal
  distance-fn is called with from and to nodes and should return a numeric distance.
  heuristic-fn is called with a node and should return a numeric estimated distance to the nearest goal.
  neighbors-fn is called with the node and should return all the neighbors
  max-depth is the maximum depth of the path in steps."
  [start goal?-fn distance-fn heuristic-fn neighbors-fn max-depth]
  (loop [open (priority-map [start [start] 0] (heuristic-fn start))
         depth 0]
    (let [[[node path distance] total] (first open)]
      (cond (> depth max-depth)
            false
            (goal?-fn node)
            {path distance}
            :else (recur (into (pop open)
                               (for [neighbor (neighbors-fn node)]
                                 (let [new-node neighbor
                                       new-path (conj path neighbor)
                                       new-distance (+ distance (distance-fn node neighbor))]
                                   [[new-node new-path new-distance]
                                    (+ new-distance (heuristic-fn neighbor))])))
                         (count path))))))
(defn dijkstra
  "Calculate shortest path with Dijkstra's algorithm.
  start is the first node.
  goal?-fn is called for each expanded node to check whether it is a goal
  distance-fn is called with from and to nodes and should return a numeric distance.
  neighbors-fn is called with the node and should return all the neighbors
  max-depth is the maximum depth of the path in steps."
  [start goal?-fn distance-fn neighbors-fn max-depth]
  ;; Dijkstra is a special case of A* when the heuristic is zero
  (a* start goal?-fn distance-fn (constantly 0) neighbors-fn max-depth))

(def paths-in-one-direction
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
(def paths (into paths-in-one-direction (map reverse-path paths-in-one-direction)))


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

(a* "a" (goal= "k") distance heuristic neighbors 20)
(dijkstra "a" (goal= "k") distance neighbors 20)
