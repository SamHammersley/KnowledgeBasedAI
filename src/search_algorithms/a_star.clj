(ns search-algorithms.a-star
  (:require [search-algorithms.graph-generation :refer :all]))

;; best first search mechanism
;; based on earlier breadth-1st search
;; @args start start state default cost 0 or given cost
;; @args goal goal state
;; @args LMG legal move generator function which takes one state & returns
;;           a list of states (typically these new states will have a cost
;;           associated with them)
;; @args selector takes list of states & selects the next one to explore from
;; @args get-state removes cost information from a state returning the raw state
;; @args get-cost  removes other information from a state returning only the cost
;; @args debug prints some information

(defn A*search
  [start goal LMG & {:keys [get-state get-cost selector debug]
                     :or   {get-state :state
                            get-cost  :cost
                            selector  :undef
                            debug     false}}]

  (let [goal-node (if (map? goal) goal {:state goal :cost 0})
        member? (fn [x lis] (some (partial = x) lis))
        selector (if (= selector :undef) (fn [bag] (first (sort-by (comp get-cost last) bag))) selector)]

    (loop [queued `((~goal-node)) visited nil]
      (if (empty? queued) nil                               ;; fail if (null queued)
                          (let [next (selector queued)      ;; select next nodes
                                state (first next)          ;; filter out path
                                raw-state (get-state state)] ;; filter costs, etc

                            (when debug (println 'selecting next '=> raw-state))

                            (cond
                              (= raw-state start)            ;; goal found
                              next            ;; quit with result (add d* function)
                              :else
                              (if (member? raw-state visited) ;; if we've visited the current state already
                                (recur (remove #(= % next) queued) visited) ;; recur removing the already visited path

                                (let [queued (remove #(= % next) queued)
                                      moves (LMG state)     ;; possible moves from the current state
                                      new-visited (cons raw-state visited) ;; visit the current state
                                      non-visited-moves (remove #(member? (get-state %) visited) moves)
                                      new-states (map #(cons % next) non-visited-moves)]

                                  (when debug (println 'exploring state '=> raw-state 'path next 'moves moves))

                                  (recur (concat queued new-states) new-visited)
                                  ))
                              ))
                          ))))

(defn d*Part
      [path LMG goal selector]
      (loop [next path]
            (cond
              (nil? (rest next))
                path
              (= (first (rest next)) (selector (LMG (first next))))
                (recur (rest next))
              :else
                (recur (A*Search ((first next) goal LMG :selector selector)))
              )))

(def graph (generate-graph 5 10 25))

(println graph)

(defn a*lmg [s]
  (let [current-state (:state s) next-states (graph current-state)]
    (map (fn [x] {:state (first x) :cost (last x)}) next-states)))

(time (A*search :a :e a*lmg))

;(println (map #(:state %) path) '=> (reduce + (map #(:cost %) path)))
;(println path '=> (reduce + (map #(:cost %) path)))
