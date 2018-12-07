(ns day5.part1
  (:require [utils.general :as utils]
            [clojure.string :as clj.str]))

(def test-polymer "dabAcCaCBAcCcaDA")
(def test-polymer-res "dabCBAcaDA") ; count = 10

(defn reaction-reducer [old curr]
  "Returns a string with the found reaction if one is found. If none is found returns the last character.
   To know if a reaction was found, test against the return value being a char (not found) or a string (found)"
  (if (utils/opposite-cases-equal? old curr)
    (reduced (str old curr))
    curr))

(defn find-reaction [str]
  (let [reaction (reduce reaction-reducer str)]
    ; ensuring that the return value is false when there is no reaction
    (if (char? reaction)
      ; if the return is a char only, it is the normal return of the reaction reducer and no reaction was found
      false
      reaction)))

(defn naive [str]
  "Recursive solution that loops over the string while it has reactions, always removing the first one found until there are none left"
  (let [toremove (find-reaction str)]
    (if-not toremove
      str
      (recur (clj.str/replace str toremove "")))))

(defn testpart1 []
  (naive test-polymer))

(defn solvepart1 []
  (->
;   expecting only a string
   (utils/read-input (fn [acc curr] (conj acc curr)) [] "src/day5/input.txt")
   (first)
   (naive)
   (count)
   ))

; solution: 10878