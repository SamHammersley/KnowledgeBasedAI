(ns search-algorithms.new-graph-generation
  (:use [search-algorithms.helper-functions]))

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

(defn min-probability-threshold [n] (/ (java.lang.Math/log n) n))

(defn generate-heuristics [goal search-algorithm distance-fn neighbours-fn]
  (let [nodes (keys neighbours-fn)]
    (if (not (in? nodes goal))
      (println goal "is not in" nodes "something has gone horribly wrong!")
      (loop [node (first nodes)
             remaining-nodes (rest nodes)
             heuristics {}]
        (if (nil? node)
          heuristics
          (recur (first remaining-nodes) (rest remaining-nodes) (assoc heuristics node (last (search-algorithm node goal distance-fn 0 neighbours-fn)))))))))