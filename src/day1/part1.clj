(ns day1.part1)

; dont judge for the file hacks pls lol
(use 'clojure.java.io)

(defn mysum [curr next]
  (+ curr (read-string next)))

(with-open [rdr (reader "src/day1/input.txt")]
      (reduce mysum 0 (line-seq rdr)))

; part 1 answer: 433