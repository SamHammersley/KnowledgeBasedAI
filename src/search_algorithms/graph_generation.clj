(ns search-algorithms.graph-generation)

(defn update-edges [graph origin-node destination-node cost]
  (println origin-node destination-node cost)
  (update (update graph origin-node conj {destination-node cost}) destination-node conj {origin-node cost}))

(defn in? [coll e]
  (some #(= e %) coll))

(defn available-nodes [graph current-node]
  (remove #(or (= % current-node) (in? (keys (graph current-node)) %)) (keys graph)))

(defn generate-edges [current-node graph min-cost max-cost probability]
  (loop [nodes (available-nodes graph current-node), edges graph, p (rand)]
     (if (nil? (first nodes))
       edges
       (let [cost (+ min-cost (rand-int (- max-cost min-cost)))
             new-graph (if (< p probability) (update-edges edges current-node (first nodes) cost) edges)]
         (recur (rest nodes) new-graph (rand))))
     ))

(def alphabet-start-index 97)

(defn generate-nodes [n]
  (loop [nodes {} current-node-index 0]
    (let [keyword (keyword (str (char (+ current-node-index alphabet-start-index))))]
      (if (= current-node-index n)
        nodes
        (recur (conj nodes {keyword {}}) (inc current-node-index))))))

(defn generate-graph [n min-cost max-cost probability]
  (loop [graph (generate-nodes n) nodes (keys graph)]
    (let [current-node (first nodes)]
      (if (nil? current-node)
        graph
        (recur (conj graph (generate-edges current-node graph min-cost max-cost probability)) (rest nodes))))))

(defn min-probability-bound [n] (/ (java.lang.Math/log n) n))

(defn generate-hopefully-connected-graph [n min-cost max-cost] (generate-graph n min-cost max-cost (min-probability-bound n)))