(ns search-algorithms.core-test
  (:require [clojure.test :refer :all]
            [search_algorithms.helper-functions :refer :all]))

(declare graph1)

(def graph1
                  {"02" 2
                   "04" 4
                   "05" 5
                   "14" 1
                   "15" 3
                   "23" 5
                   "24" 6
                   "45" 2})

(defn test-reverse-path [[k v]] [(apply str (reverse k)) v])
(def test-paths (into graph1 (map test-reverse-path graph1)))
(def test-neighbors (apply merge-with concat (map (fn [[[from to] test-distance]] {(str from) [(str to)]}) test-paths)))
(def test-distance (comp paths str))

(def test-heuristic {
                "0" 3
                "1" 3
                "2" 2
                "3" 3
                "4" 2
                "5" 2})


(deftest graph1-test
  (is (= ["5" "0" "2" "3"] (a* "5" "3" test-distance test-heuristic test-neighbors)))
  ;(println (a* "5" "3" test-distance test-heuristic test-neighbors))

  (is (= ["5" "0" "2" "3"] (d* "5" "3" test-distance test-heuristic test-neighbors)))
  ;(println (d* "5" "3" test-distance test-heuristic test-neighbors))

  (is (= ["5" "0" "2" "3"] (dijkstra "5" "3" test-distance test-heuristic test-neighbors)))
  ;(println (dijkstra "5" "3" test-distance test-heuristic test-neighbors))
  )

(deftest graph1vals
  (time (for [x (range 0 1000)] (a* "5" "3" test-distance test-heuristic test-neighbors)))
  )
(graph1-test)

;(def graph2
;  {"0" "2" 11
;       "0" "4" 1
;   "0" "5" 7
;       "1" "4" 4
;   "1"  "5" 8
;   "2" "3" 9
;   "4"  "5" 2
;   "7" "9" 3
;   "13" "8" 4
;   "17" "23" 10
;   "10" "25" 21
;   "24" "18" 13
;   "16" "14" 15
;   "20" "15" 16
;   "22" "21" 17
;   "11" "22" 23
;   "11" "20" 26
;   "15" "22" 20
;   "19" "2" 2
;   "6" "24" 7
;   "10" "1" 4
;   "7" "0" 8
;   "9" "0" 2
;   "13" "7" 9
;   "8" "9" 7
;   "17" "7" 2
;   "13" "23" 1
;   "17" "10" 7
;   "0" "22" 26
;   "2" "11" 21
;   "19" "12" 5
;   "14" "6" 7
;   "16" "23" 3
;   "18" "25" 4})
;
;(def graph3
;{"0" "2" 7
;     "0" "4" 3
; "0" "5" 21
;     "1" "4" 7
; "1" "5" 8
;     "2" "3" 15
; "2" "4" 21
;     "4" "5" 15
; "49" "25" 11
; "48" "13" 1
; "25" "48" 8
; "26" "13" 10
; "48" "47" 4
; "24" "13" 6
; "24" "45" 8
; "13" "45" 7
; "45" "26" 13
; "47" "2" 16
; "24" "2" 29
; "49" "47" 10
; "6" "3" 19
; "6" "47" 33
; "27" "49" 24
; "27" "6" 11
; "28" "27" 12
; "28" "6" 6
; "29" "28" 4
; "6" "29" 2
; "1" "3" 15
; "46" "24" 17
; "46" "45" 16
; "23" "12" 13
; "23" "46" 11
; "44" "23" 9
; "43" "23" 31
; "42" "43" 35
; "12" "46" 7
; "44" "43" 21
; "42" "44" 27
; "22" "44" 3
; "22" "12" 5
; "12" "24" 28
; "41" "42" 21
; "41" "0" 7
; "22" "2" 7
; "14" "42" 4
; "40" "14" 5
; "42" "40" 21
; "40" "41" 33
; "15" "40" 22
; "11" "41" 12
; "11" "21" 11
; "0" "21" 16
; "5" "21" 9
; "39" "21" 8
; "39" "5" 5
; "39" "11" 2
; "20" "11" 4
; "16" "38" 14
; "38" "20" 17
; "20" "10" 18
; "10" "39" 22
; "10" "5" 19
; "16" "10" 21
; "18" "17" 25
; "17" "36" 7
; "36" "16" 9
; "37" "9" 40
; "9" "19" 13
; "19" "35" 32
; "35" "1" 46
; "33" "32" 4
; "32" "29" 1
; "1" "33" 2
; "33" "29" 8
; "29" "3" 16
; "28" "32" 17
; "7" "33" 15
; "7" "8" 3
; "31" "8" 12
; "34" "8" 18
; "8" "33" 10
; "7" "30" 11
; "30" "28" 12
; "31" "7" 19
; "34" "9" 3
; "37" "36" 5}
;  )