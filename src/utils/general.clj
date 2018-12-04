(ns utils.general)

(use 'clojure.java.io)

(defn read-input [func start-val filename]
  "Read input from filename and reduce it"
  (with-open [rdr (reader filename)]
      (reduce func start-val (line-seq rdr))))

; sadly not baked into clojure
(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))