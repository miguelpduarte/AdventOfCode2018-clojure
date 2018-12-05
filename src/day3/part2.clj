(ns day3.part2
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

(def unparsedclaim1 "#1 @ 1,3: 4x4")
(def parsedclaim1 {:id 1, :left 1, :top 3, :width 4, :height 4})

(def testclaims ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])
(def cena (into #{} (map parse-claim testclaims)))

(defn claim-overlaps? [claim1 claim2]
  ; Axis Aligned Bounding Box (lmao)
  (and (< (:left claim1) (+ (:left claim2) (:width claim2)))
       (> (+ (:left claim1) (:width claim1)) (:left claim2))
       (< (:top claim1) (+ (:top claim2) (:height claim2)))
       (> (+ (:top claim1) (:height claim1)) (:top claim2))
       ))

(defn reducer-claim-overlaps? [current passed]
  "For use in reduce, initial value should be the the value to check against the array, and passed is the currently passed value array
   Call like (reduce reducer-claim-overlaps? claim-to-check array-to-check-against)
   Will return true as soon as something overlapping is found, current value otherwise"
  (if (claim-overlaps? current passed)
    (reduced true)
    current))

(defn recalc-candidates [newcandidate candidates]
  "Returns a map of the new candidates that were rejected and the ones that are still good: {:good #{c1 c2 c3} :bad #{o1 o2 o3}} based on the new claim to check against"
  (let [newcandidatesmap (reduce (fn [candidatemap currcandidate]
                                   (if (claim-overlaps? newcandidate currcandidate)
                                     ; both are bad if they overlap
                                     (assoc candidatemap :bad (conj (:bad candidatemap) currcandidate newcandidate))
                                     ; it was already in the map so it continues to not coincide with anything
                                     (assoc candidatemap :good (conj (:good candidatemap) currcandidate))))
                                 {:good #{} :bad #{}} candidates)]
    newcandidatesmap))

(defn update-currmap [currmap newcandidatesmap]
  "Updates the current overlapping and candidates map with the new candidates information"
  (let [varcenas {:overlapping (apply conj (:overlapping currmap) (:bad newcandidatesmap))
                  :candidates (:good newcandidatesmap)}]

    ; (println "resultado do update" varcenas)
    ; (println "overlapping atual" (:overlapping currmap))
    ; (println "bad do novo cand" (:bad newcandidatesmap))
    ; (println "cand atual" (:candidates currmap))
    ; (println "good novo cand" (:good newcandidatesmap))

    varcenas))

(defn add-if-not-overlap [currmap newclaim]
  (let [{:keys [overlapping candidates]} currmap]
    (if (empty? candidates)
      (assoc currmap :candidates #{newclaim})
      ; an improvement would be if it was in bad then it would go into overlapping, thus skipping the check there but meh functional programming is hard xD
      (let [overlaps-with-overlapping (reduce reducer-claim-overlaps? newclaim overlapping)
            aux-newcandidatesmap (recalc-candidates newclaim candidates)]

        ; (println "currm" currmap) (println "o-w-o" overlaps-with-overlapping "a-ncm" aux-newcandidatesmap "nc" newclaim)

        (if (true? overlaps-with-overlapping)
          ; our newclaim might not be in :bad if it did not collide with any claim
          (update-currmap currmap (assoc aux-newcandidatesmap :bad (conj (:bad aux-newcandidatesmap) newclaim)))
          ; our newclaim might be a new candidate
          (if-not (contains? (:bad aux-newcandidatesmap) newclaim)
            ; if it is not in the :bad set, then it is a new candidate
            (update-currmap currmap (assoc aux-newcandidatesmap :good (conj (:good aux-newcandidatesmap) newclaim)))
            ; otherwise it is in the bad set and will be added to the :overlapping anyway
            (update-currmap currmap aux-newcandidatesmap)))))))


; final test with example
(def test1res '(3))

(defn test1 []
  (->>
   testclaims
   (map parse-claim)
   (reduce add-if-not-overlap {:overlapping #{} :candidates #{}})
   (:candidates)
   (map :id)
   ))

; (= (test1) test1res)

(defn solvepart2 []
  (->>
   ; ugly, i know, im sorry
   (utils/read-input (fn [acc curr] (conj acc (parse-claim curr))) [] "src/day3/input.txt")
  ;  (take 300)
   ; now we have a seq with the parsed claims
   (reduce add-if-not-overlap {:overlapping #{} :candidates #{}})
   (:candidates)
   (map :id)
   ))

; solution: 