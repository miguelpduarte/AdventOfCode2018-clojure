(ns aocd2p1)

(use 'clojure.java.io)

(defn string-to-map [map char]
  (if (nil? (map char))
    (conj map [char 1])
    (conj map [char (inc (map char))])))

(defn eval-id-aux [[val1 val2] [_char occurrences]]
  (if (= occurrences 2)
    [1 val2]
    (if (= occurrences 3)
      [val1 1]
      [val1 val2]
      )
    )
  )

(defn eval-id [id]
  (let [parsed-id (reduce string-to-map {} id)]
    (reduce eval-id-aux [0 0] parsed-id)
    ))

(defn sum-all-evals [[last-val1 last-val2] currentline]
  (let [[val1 val2] (eval-id currentline)]
    [(+ val1 last-val1) (+ val2 last-val2)]
    )
  )

(defn read-input [func start-val]
  (with-open [rdr (reader "day2/input.txt")]
    (reduce func start-val (line-seq rdr))))

(defn solvepart1 []
  (let [[val1-final val2-final] (read-input sum-all-evals [0 0])]
    (* val1-final val2-final)
    )
  )

; solution 7134