(ns day5.part2
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

(defn generate-all-without-one-type [str]
  "Returns a vector of all polymers with one type removed (all strings without one character - both upper and lower case)"
  (let [unique-chars-in-str (into #{} str)]
    ; After we have all the unique characters in the passed string, we proceed to removing each of them individually
    (reduce (fn [acc curr]
              ; acc is the current vector of generated strings, curr is the current char we are removing
              (let [remover (utils/create-re-pattern-both-cases curr)]
                ; removing the matches in the original string and append it to the current result
                (conj acc (clj.str/replace str remover ""))
                ))
            [] unique-chars-in-str)
    ))

(generate-all-without-one-type test-polymer)

(defn testpart2 []
  (as->
   test-polymer input
   (generate-all-without-one-type input)
   (pmap naive input)
   (pmap count input)
   (apply min input)
   )
  )

(defn solvepart2 []
  (as->
   (utils/read-input (fn [acc curr] (conj acc curr)) [] "src/day5/input.txt") input
    ; expecting only one string in the input
    (first input)
    (generate-all-without-one-type input)
    (map naive input)
    (map count input)
    (apply min input)
    ))

(defn solvepart2withcutbefore []
  (as->
   (utils/read-input (fn [acc curr] (conj acc curr)) [] "src/day5/input.txt") input
    ; expecting only one string in the input
    (first input)
    (naive input)
    (generate-all-without-one-type input)
    (mapv naive input)
    (mapv count input)
    (apply min input)))

(defn solvepart2v2 []
  (as->
   (utils/read-input (fn [acc curr] (conj acc curr)) [] "src/day5/input.txt") input
    ; expecting only one string in the input
    (first input)
    ; (re-naive input)
    (generate-all-without-one-type input)
    (map re-naive input)
    (map count input)
    (apply min input)))

; solution: 6874

;Interesting tests (all timed using "time"):
; Test input (very small test case, results very inconsistent):
; time with map: "Elapsed time: 1.628136 msecs"
; time with mapv: "Elapsed time: 1.451469 msecs"
; time with pmap: "Elapsed time: 3.919321 msecs"
;;;;;;
; Final solution (actual input):
; time with map: "Elapsed time: 45575.522766 msecs"
; time with mapv: "Elapsed time: 45465.739832 msecs"
; time with pmap: "Elapsed time: 16123.127072 msecs"
;;;;;;
; Final solution with cut before (actual input):
; time with map: "Elapsed time: 5433.901875 msecs"
; time with mapv: "Elapsed time: 5474.966432 msecs"
; time with pmap: "Elapsed time: 5091.002632 msecs"
;;;;;;
; v2:
; time with map: "Elapsed time: 69278.965242 msecs"
; time with mapv: 
; time with pmap: 