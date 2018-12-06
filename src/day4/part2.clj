(ns day4.part2
  (:require [utils.general :as utils]))

(def record_regexp #"^\[(?<date>\d\d\d\d-\d\d-\d\d) (?<time>(?:\d\d):(?<mins>\d\d))] (?<text>(?:[\w\d#]\s?)+)$")
(def id_regexp #"^.*#(?<id>\d+).*$")

(defn parse-record [str]
  (let [matcher (re-matcher record_regexp str)]
    (if (.matches matcher)
      {:date (.group matcher "date")
       :time (.group matcher "time")
       :mins (utils/parse-int (.group matcher "mins"))
       :text (.group matcher "text")})))

(defn id-from-shift-start [str]
  (let [matcher (re-matcher id_regexp str)]
    (if (.matches matcher)
      (utils/parse-int (.group matcher "id"))
      )))

(defn update-guard-minutemap [minutemap currminute]
  "Increments the minute if it exists, adds an entry with value 1 if it does not. Call like (reduce update-guard-minutemap minutemap minuterange)"
  (if (get minutemap currminute)
    (update-in minutemap [currminute] inc)
    (assoc minutemap currminute 1)))

(defn update-guard [[sleepacc, lastsleepmin, minutemap], wakeup-minute]
  "Updates the given guard data after it wakes up at the given minute. Arguments are the guard to update and the wakeup minute"
  (let [sleepminsrange (range lastsleepmin wakeup-minute)
        newminutemap (reduce update-guard-minutemap minutemap sleepminsrange)]
    ; (println "from" lastsleepmin "to" wakeup-minute " range:" sleepminsrange "\noldminmap:" minutemap "\nnewminmap:" newminutemap)
    [(+ sleepacc (- wakeup-minute lastsleepmin)), nil, newminutemap]))

(def test-records ["[1518-11-01 00:00] Guard #10 begins shift" "[1518-11-01 00:05] falls asleep" "[1518-11-01 00:25] wakes up" "[1518-11-01 00:30] falls asleep" "[1518-11-01 00:55] wakes up"
"[1518-11-01 23:58] Guard #99 begins shift" "[1518-11-02 00:40] falls asleep" "[1518-11-02 00:50] wakes up" "[1518-11-03 00:05] Guard #10 begins shift" "[1518-11-03 00:24] falls asleep"
"[1518-11-03 00:29] wakes up" "[1518-11-04 00:02] Guard #99 begins shift" "[1518-11-04 00:36] falls asleep" "[1518-11-04 00:46] wakes up" "[1518-11-05 00:03] Guard #99 begins shift"
"[1518-11-05 00:45] falls asleep" "[1518-11-05 00:55] wakes up"])

(defn reduce-records [currmap {:keys [text mins]}]
  ; (println currmap)
  ; (println "text:" text "| mins:" mins)
  (case text
    ; update lastsleepmin for the current guard
    "falls asleep"
    (let [currguardid (:currguardid currmap), currguard (get currmap currguardid [0 nil {}])]
      ; (println "cgid:" currguardid, "| currguard:" currguard)
      (assoc currmap currguardid (assoc currguard 1 mins)))
    
    ; calculate the sleeping time and increment the sleepacc
    "wakes up"
    (let [currguardid (:currguardid currmap), currguard (get currmap currguardid), updatedguard (update-guard currguard mins)]
      ; (println "cgid:" currguardid, "| updatedguard:" updatedguard)
      (assoc currmap currguardid updatedguard))
    
    ; else case is that a guard started his shift, store the id in the currguardid key of the map
    (do
      ; (println "id" (id-from-shift-start text))
      (assoc currmap :currguardid (id-from-shift-start text))
      )
    ))

(defn get-most-frequent-minute [[best-id, best-minute, best-frequency] candidate-id [_sleepacc _lastsleeptime minutemap]]
  "Returns the most frequently asleep guard - the guard id, minute, and frequency in a vec: [id, minute, frequency]"
  (let [[candidate-minute candidate-frequency] (apply max-key val minutemap)]
    (if (> candidate-frequency best-frequency)
      [candidate-id candidate-minute candidate-frequency]
      [best-id best-minute best-frequency])))

(defn testpart2 []
  (let [sorted-records (->> test-records (map parse-record) (sort-by (juxt :date :time)))]
  ; Storing currguardid because there is no id indication in falling asleep or waking up
  ; {:currguardid curr_guard_id, id [sleepacc lastsleepmin, {minute frequency}]], id2 [sleepacc2 lastsleepmin], ...}
    (as-> sorted-records myval
      ; transforming the parsed records into guard information
      (reduce reduce-records {} myval)
      ; removing the temporary :currguardid key
      (dissoc myval :currguardid)
      ; using these starting values to simplify first iteration
      ; [guard-id, most-frequent-minute, largest-frequency]
      (reduce-kv get-most-frequent-minute [0, 0, -1] myval)
      (let [sol-id (first myval), minute (second myval), res (* sol-id minute)]
        {:id sol-id :minute minute :res res}))))

(defn solvepart2 []
  (as->
   (utils/read-input (fn [acc curr] (conj acc (parse-record curr))) [] "src/day4/input.txt") myval

    (sort-by (juxt :date :time) myval)
    (reduce reduce-records {} myval)
    (dissoc myval :currguardid)
      ; using these starting values to simplify first iteration
      ; [guard-id, most-frequent-minute, largest-frequency]
    (reduce-kv get-most-frequent-minute [0, 0, -1] myval)
    (let [sol-id (first myval), minute (second myval), res (* sol-id minute)]
      {:id sol-id :minute minute :res res})))