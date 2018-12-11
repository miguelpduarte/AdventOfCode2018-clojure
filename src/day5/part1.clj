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

(defn remove-with-re [str]
  (clj.str/replace str #"aA|Aa|bB|Bb|cC|Cc|dD|Dd|eE|Ee|fF|Ff|gG|Gg|hH|Hh|iI|Ii|jJ|Jj|kK|Kk|lL|Ll|mM|Mm|nN|Nn|oO|Oo|pP|Pp|qQ|Qq|rR|Rr|sS|Ss|tT|Tt|uU|Uu|vV|Vv|wW|Ww|xX|Xx|yY|Yy|zZ|Zz" ""))

(defn re-naive [str]
  (let [newstr (remove-with-re str)]
    (if (= str newstr)
      str
      (recur newstr))))

(defn testpart1 []
  (naive test-polymer))

(defn solvepart1 []
  (->
   ; expecting only a string
   (utils/read-input (fn [acc curr] (conj acc curr)) [] "src/day5/input.txt")
   (first)
   (naive)
   (count)))

; solution: 10878

(defn solvepart1angelo []
  (->
   ; expecting only a string
   (utils/read-input (fn [acc curr] (conj acc curr)) [] "src/day5/input_angelo.txt")
   (first)
   (naive)
   (count)
   ))

;times:
; v1: "Elapsed time: 1472.221379 msecs"
; v2: "Elapsed time: 2461.25008 msecs"

(defn solvepart1v2 []
  (->
;   expecting only a string
   (utils/read-input (fn [acc curr] (conj acc curr)) [] "src/day5/input.txt")
   (first)
   (re-naive)
   (count)))