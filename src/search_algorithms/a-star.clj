(ns search-algorithms.a-star)

;; best first search mechanism
;; based on earlier breadth-1st search
;; @args start start state
;; @args goal either a predicate to take a state determine if it is a goal
;;            or a state equal to the goal
;; @args LMG  legal move generator function which takes one state & returns
;;            a list of states (typically these new states will have a cost
;;            associated with them)
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

  (let [goal? (if (fn? goal) #(when (goal %) %) #(when (= % goal) %))
        member? (fn [x lis] (some (partial = x) lis))
        selector (if (= selector :undef) (fn [bag] (first (sort-by (comp get-cost first) bag))) selector)]

    (loop [queued `((~start)) visited nil]
      (if (empty? queued) nil                               ;; fail if (null queued)
                          (let [next (selector queued)      ;; select next node
                                state (first next)          ;; filter out path
                                raw-state (get-state state)] ;; filter costs, etc

                            (when debug (println 'selecting (reverse next) '=> raw-state))

                            (cond
                              (goal? raw-state)             ;; goal found
                                (reverse next)              ;; quit with result
                              :else
                                (if (member? raw-state visited) ;; if we've visited the current state already
                                  (recur (remove #(= % next) queued) visited) ;; recur removing the already visited path

                                  (let [queued (remove #(= % next) queued)
                                        moves (LMG state) ;; possible moves from the current state
                                        new-visited (cons raw-state visited) ;; visit the current state
                                        non-visited-moves (remove #(member? (get-state %) visited) moves)
                                        new-states (map #(cons % next) non-visited-moves)]

                                    (when debug (println 'exploring state '=> raw-state 'path next 'moves moves))

                                    (recur (concat queued new-states) new-visited)
                                    ))
                              ))
                          ))))

(defn map-invert [m] (reduce (fn [m [k v]] (assoc m v k)) {} m))

(def graph { :a [:b :c],
             :b [:a :c :d],
             :c [:a :b],
             :d [:b :e],
             :e [:b :d]
            })

(def weights {
              {:a :b} 10,
              {:a :c} 8,
              {:b :d} 5,
              {:b :c} 9,
              {:b :e} 6,
              {:d :e} 4
            })

(defn get-weighting [x y] (let [key {x y}] (get weights key (weights (map-invert key)))))

(defn a*lmg [s] (let [current-state (:state s) next-states (graph current-state)]
                  (map (fn [x] {:state x, :cost (get-weighting x current-state)}) next-states)))

(A*search {:state :a :cost 0} :e a*lmg :debug true)
