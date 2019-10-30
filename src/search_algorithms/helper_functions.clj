(ns search-algorithms.helper-functions)

(defn in? [coll e]
  (some #(= e %) coll))

(defn ^:private format-edges [edges] (for [kv edges] (clojure.string/join " " (str (key kv) (val kv)))))

(defn print-formatted-edges [edges] (doseq [x (format-edges edges)] (println x)))

(defn debug [path] (println path) path)
(defn reverse-path [[k v]] [(apply str (reverse k)) v])