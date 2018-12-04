(ns day2.part2
  (:require [clojure.string :as str :refer :all]))

(use 'clojure.java.io)

(defn string-close? [str1 str2]
  (->>
   (map = str1 str2)
   (filter not)
   (count)
   (= 1)
   ))

(defn get-common-chars [str1 str2]
  ; just training my use of as->
  (as-> (fn [char] (str/index-of str1 char)) val
    (filter val str2)
    (apply str val)))

; checks current against all of the elements in "others" successively
(defn check-all-for-one [current others]
  (when-not (empty? others)
    (let [candidate (first others)]
      (if (string-close? current candidate)
        (get-common-chars current candidate)
        (check-all-for-one current (rest others))))))

(defn cross-and-check [lines]
  (when-not (empty? lines)
    (let [res (check-all-for-one (first lines) (rest lines))]
  ; when a match is found, a string with the common characters is returned (return it outwards)
      (if (string? res)
        res
        (cross-and-check (rest lines))))))

(defn read-input [func]
  (with-open [rdr (reader "day2/input.txt")]
    (let [lines (line-seq rdr)]
         (func lines))))

(def testinput ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(defn solvepart2 []
  (read-input cross-and-check))

(defn testpart2 []
  (cross-and-check testinput))

;solution "kbqwtcvzhmhpoelrnaxydifyb"