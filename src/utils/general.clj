(ns utils.general
    (:require [clojure.string :as clj.str]))

(use 'clojure.java.io)

(defn read-input [func start-val filename]
  "Read input from filename and reduce it"
  (with-open [rdr (reader filename)]
      (reduce func start-val (line-seq rdr))))

; sadly not baked into clojure
(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn is-lower? [c]
  (not (nil? (re-find #"^[a-z]$" (str c)))))

(defn is-upper? [c]
  (not (nil? (re-find #"^[A-Z]$" (str c)))))

(defn opposite-cases? [c1 c2]
  (or
   (and (is-lower? c1) (is-upper? c2))
   (and (is-upper? c1) (is-lower? c2))))

(defn opposite-cases-equal? [c1 c2]
  (and
   (opposite-cases? c1 c2)
   (= (clj.str/upper-case c1) (clj.str/upper-case c2))
   ))

(defn create-re-pattern-both-cases [c]
  "Creates a RE pattern that matches both cases of the passed character"
  (re-pattern (apply str "[" c (clj.str/upper-case c) "]"))
  )

(defn val-in-range [val rangelow rangehigh]
  "True if val is inside the given range, false otherwise (inclusive)"
  (and (>= val rangelow)
       (<= val rangehigh)))