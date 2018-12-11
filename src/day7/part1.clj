(ns day7.part1
  (:require [utils.general :as utils]))

(def test-requirements ["Step C must be finished before step A can begin." "Step C must be finished before step F can begin." "Step A must be finished before step B can begin." "Step A must be finished before step D can begin." "Step B must be finished before step E can begin." "Step D must be finished before step E can begin." "Step F must be finished before step E can begin."])
(def test-requirements-res "CABDFE")

(def requirement_regexp #"^Step (?<needs>\S) must be finished before step (?<id>\S) can begin\.$")

(defn str->requirement [str]
  "Converts a requirement string to [id, needsid]"
  (let [matcher (re-matcher requirement_regexp str)]
    (if (.matches matcher)
      [(.group matcher "id")
       (.group matcher "needs")])))

(defn parse-requirement-reducer [currmap str]
  "Reduces the passed requirement strings to a map of id and vector of required steps"
  (let [[id, needs] (str->requirement str)
        ; Appending the new "needs"
        updatedmap (assoc currmap id (conj (get currmap id []) needs))]
    
  ; Also inserting the needs id wih an empty vector if it does not exists, so that the ids without requirements also exist in the map
    (if-not (get updatedmap needs)
      (assoc updatedmap needs [])
      updatedmap)))

(defn get-without-requirements [acc currk currv]
  "Returns the current steps that have no requirements"
  (if (empty? currv)
    (conj acc currk)
    acc))

(defn update-map [next-step]
  "Returns a function that removes the specified next-step from the required steps of all steps that have it (for use in reduce-kv)"
  (fn [acc currk currv]
    (conj acc [currk, (filter #(not= % next-step) currv)])))

(defn determine-order [currmap solution-str]
  (if (empty? currmap)
    solution-str
    (let [next-step (first (sort (reduce-kv get-without-requirements [] currmap)))
          currmap-without-next-step (dissoc currmap next-step)
          updated-map (reduce-kv (update-map next-step) {} currmap-without-next-step)]
      (recur updated-map (apply str solution-str next-step)))))

(defn testpart1 []
  (as->
   test-requirements data
   (reduce parse-requirement-reducer {} data)
   (determine-order data "")
   ))

(defn solvepart1 []
  (as->
   (utils/read-input parse-requirement-reducer {} "src/day6/input.txt") input
   (determine-order input "")))

; solution: PFKQWJSVUXEMNIHGTYDOZACRLB