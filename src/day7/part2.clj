(ns day7.part2
  (:require [utils.general :as utils]))

(def test-requirements ["Step C must be finished before step A can begin." "Step C must be finished before step F can begin." "Step A must be finished before step B can begin." "Step A must be finished before step D can begin." "Step B must be finished before step E can begin." "Step D must be finished before step E can begin." "Step F must be finished before step E can begin."])
(def test-requirements-res2 "CABFDE")

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

(defn get-time-from-letter [letter curr-tick]
  (+ curr-tick 59 (int (.charAt letter 0)) -64))

(defn get-not-in [vec1 vec2]
  (filterv #(= (.indexOf vec1 %) -1) vec2))

(defn finish-working [curr-tick workers]
  ; {:1 ["A" 63]}
  ; return results: [new-workers finished-steps]
  (reduce-kv (fn [[curr-new-workers curr-finished-steps] worker-k worker-v]
               (if (empty? worker-v)
                 ; Not assigned at all
                 [(conj curr-new-workers [worker-k worker-v]) curr-finished-steps]
                 (let [[letter time] worker-v]
                   (if (<= time curr-tick)
                 ; done
                     [(conj curr-new-workers [worker-k []]) (conj curr-finished-steps letter)]
                 ; not done
                     [(conj curr-new-workers [worker-k worker-v]) curr-finished-steps])))
               ) [{} []] workers)
  )

(defn start-working [curr-tick workers steps-to-do]
  "Returns [[] new-workers-map]"
  (if (empty? steps-to-do)
    [[] workers]
    (reduce-kv (fn [[steps-to-do acc-workers] worker-k worker-v]
                 (if (empty? steps-to-do)
                   ; Cannot assign any more so will just concat
                   [[] (conj acc-workers [worker-k worker-v])]
                   (if (empty? worker-v)
                   ; Available, start working
                     [(rest steps-to-do) (conj acc-workers [worker-k [(first steps-to-do) (get-time-from-letter (first steps-to-do) curr-tick)]])]
                   ; Not available, continue iterations
                     [steps-to-do (conj acc-workers [worker-k worker-v])])
                   )
                 )[steps-to-do {}] workers)
    ))

(defn get-without-requirements [acc currk currv]
  "Returns the current steps that have no requirements"
  (if (empty? currv)
    (conj acc currk)
    acc))

(defn update-map [next-steps]
  "Returns a function that removes the specified next-steps from the required steps of all steps that have it (for use in reduce-kv)"
  (fn [acc currk currv]
    (conj acc [currk, (get-not-in next-steps currv)])))

(defn remove-done [currmap finished-steps]
  "Removes the steps from the map that have finished"
  (reduce (fn [currmap-acc curr-finished-step]
            (dissoc currmap-acc curr-finished-step)
            ) currmap finished-steps)
  )

(defn determine-order [curr-tick workers currmap solution-str]
  (if (empty? currmap)
    [curr-tick solution-str]
    (let [next-steps-full (sort (reduce-kv get-without-requirements [] currmap))
          ; test (println "1" next-steps-full)
          next-steps-not-doing (get-not-in (mapv first (vals workers)) next-steps-full)
          ; test2 (println "2")  
          [new-workers-aux finished-steps] (finish-working curr-tick workers)
          ; test3 (println "3" new-workers-aux)
          [irrelevant new-workers] (start-working curr-tick new-workers-aux next-steps-not-doing)
          ; test3 (println "4" new-workers)          
          currmap-without-finished (remove-done currmap finished-steps)
          ; test3 (println "5")          
          updated-map (reduce-kv (update-map finished-steps) {} currmap-without-finished)]
          ; test3 (println "6")]
      ; (println "ah?" curr-tick "oh:" new-workers "oi?" solution-str)
      (recur (inc curr-tick) new-workers updated-map (apply str solution-str finished-steps)))))

(defn testpart2 []
  (as->
   test-requirements data
   (reduce parse-requirement-reducer {} data)
   (determine-order 0 {:1 [] :2 []} data "")
   ))

(defn solvepart2 []
  (as->
   (utils/read-input parse-requirement-reducer {} "src/day6/input.txt") input
    ; input
    (determine-order 0 {:1 [] :2 [] :3 [] :4 [] :5 []} input "")
    ))

; solution: 