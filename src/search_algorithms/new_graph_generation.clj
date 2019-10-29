(ns search-algorithms.new-graph-generation)

(def alphabet-start-index 97)

(defn generate-edge-for-each-node [origin-node other-nodes min-cost max-cost probability-threshold]
  (let [cost (+ min-cost (rand-int (- max-cost min-cost)))]
    (loop [current-node (first other-nodes) nodes (rest other-nodes) p (rand) result {}]
      (if (nil? current-node)
          result
          (let [r (if (< p probability-threshold) (assoc result (str origin-node current-node) cost) result)]
            (recur (first nodes) (rest nodes) (rand) r))
        ))))

(defn generate-new-edges [n min-cost max-cost probability-threshold]
  (let [nodes (map (comp str char #(+ % alphabet-start-index)) (range n))]
    (loop [remaining (rest nodes)
           node (first nodes)
           edges {}]
      (if (empty? remaining)
        edges
        (let [e (generate-edge-for-each-node node (remove #(= % node) nodes) min-cost max-cost probability-threshold)]
          (recur (rest remaining) (first remaining) (conj edges e)))))))

(generate-new-edges 11 1 4 0.5)

(defn min-probability-threshold [n] (/ (java.lang.Math/log n) n))

(defn generate-heuristics [goal search-algorithm distance-fn neighbours-fn]
    (loop [nodes (keys neighbours-fn)
           heuristics {}]
      (println 'from (first nodes) 'to goal)
      (if (empty? nodes)
        heuristics
        (recur (rest nodes) (assoc heuristics (first nodes) (search-algorithm (first nodes) goal distance-fn neighbours-fn))))))