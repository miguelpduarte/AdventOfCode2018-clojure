(ns day3.part1
  (:require [utils.general :as utils]))

(def dimensions_regexp #"^#(?<id>\d+) @ (?<left>\d+),(?<top>\d+): (?<width>\d+)x(?<height>\d+)$")

(defn parse-claim [str]
  (let [matcher (re-matcher dimensions_regexp str)]
    (if (.matches matcher)
      {:id (utils/parse-int (.group matcher "id"))
       :left (utils/parse-int (.group matcher "left"))
       :top (utils/parse-int (.group matcher "top"))
       :width (utils/parse-int (.group matcher "width"))
       :height (utils/parse-int (.group matcher "height"))})))

(def val1 "#123 @ 3,2: 5x4")
(def res1 {:id 123 :left 3 :top 2 :width 5 :height 4})

; (= (parse-claim val1) res1)

(def testclaims ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])
; (map parse-claim testclaims)

(defn claim->area [{:keys [left top width height]}]
  "Converts a claim in format {:id x :left x :top x :width x :height x} to an area in the format [startx starty endx endy] (starting from the top left corner for simplicity)"
   [left top (+ left width) (+ top height)])

; (def area1 (claim->area (parse-claim test1)))

(defn add-or-inc [map x y]
  "Adds an item to a bidimensional map if it does not exist or increments its value if it does"
  (->>
   (get-in map [x y] 0)
   (inc)
   (assoc-in map [x y])
   ))

(defn add-area [areamap [startx starty endx endy]]
  (let [xrange (range startx endx) yrange (range starty endy)]
    (reduce
     (fn [currareamap curry]
       (reduce
        (fn [currareamapdeep currx]
          (add-or-inc currareamapdeep currx curry))
        currareamap xrange))
     areamap
     yrange)))

; (add-area {} area1)

(defn count-overlapping [fullareamap]
  (reduce-kv
   ; reducing the outer map to go over each line
   (fn [curr _k line]
     (reduce-kv
     ; reducing each line (the current line item value is in lineitem)
      (fn [deepcurr _k lineitem]
        ; if the count is larger than 1, the squares are overlapping, increment the overlapping square counter
        (if (> lineitem 1)
          (inc deepcurr)
          deepcurr))
      curr line))
   0 fullareamap))

; (count-overlapping {1 {1 5, 2 2}}) ; 2

; final test with example
(def test1res 4)

(defn test1 []
  (->>
   testclaims
   (map parse-claim)
   (map claim->area)
   (reduce add-area {})
   (count-overlapping)))

(= (test1) test1res)



(defn solvepart1 []
  (->>
   ; ugly, i know, im sorry
   (utils/read-input (fn [acc curr] (conj acc (parse-claim curr))) [] "src/day3/input.txt")
   ; now we have a seq with the parsed claims
   (map claim->area)
   (reduce add-area {})
   (count-overlapping)
   ))

; solution: 101469