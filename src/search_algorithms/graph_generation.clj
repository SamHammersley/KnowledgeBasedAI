(ns search-algorithms.graph-generation)

(defn in? [coll e]
  (some #(= e %) coll))

(defn random-element [coll existing]
  (if (nil? existing)
    (random-element coll '())
    (let [available (remove #(in? existing %) coll)]
      (if (nil? (first available)) nil (nth available (rand-int (count available)))))))

(defn update-edges [graph origin-node selected-node cost]
  (update (update graph origin-node conj {selected-node cost}) selected-node conj {origin-node cost}))

(defn generate-edges [origin-node graph min-cost max-cost]
  (loop [n (inc (rand-int (- (count graph) (count (graph origin-node))))) edges graph]
    (if (zero? n)
      edges
      (let [filtered-nodes (remove #(= % origin-node) (keys edges))
            selected-node (random-element filtered-nodes (keys (edges origin-node)))
            cost (+ min-cost (rand-int (- max-cost min-cost)))]
        (if (nil? selected-node)
          edges
          (recur (- n (inc (rand-int (dec n)))) (update-edges edges origin-node selected-node cost)))))))

(def alphabet-start-index 97)

(defn generate-nodes [n]
  (loop [nodes {} current-node-index 0]
    (let [keyword (keyword (str (char (+ current-node-index alphabet-start-index))))]
      (if (= current-node-index n)
        nodes
        (recur (conj nodes {keyword {}}) (inc current-node-index))))))

(defn generate-graph [n min-cost max-cost]
  (loop [graph (generate-nodes n) nodes (keys graph)]
    (let [current-node (first nodes)]
      (if (nil? current-node)
        graph
        (recur (conj graph (generate-edges current-node graph min-cost max-cost)) (rest nodes))))))