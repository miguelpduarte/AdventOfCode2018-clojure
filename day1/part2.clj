; dont judge for the file hacks pls lol
(use 'clojure.java.io)

(defn mysum [curr next]
  (let [newsum (+ (curr :sum) (read-string next))]
    (if (contains? (curr :seen) newsum)
      (reduced newsum)
      {:sum newsum :seen (conj (curr :seen) newsum)})))


(defn read-and-parse [starting-vals] (with-open [rdr (reader "day1/input.txt")]
      (reduce mysum starting-vals (line-seq rdr))))

(defn solpart2 [val]
  (if (number? val)
    val
    (solpart2 (read-and-parse val))
    ))

(defn solvepart2 []
  (solpart2 {:sum 0 :seen #{}}))

; solution 256