(ns advent2016.core
  (:use [clojure.edn :only [read-string]])
  (:require [schema.core :as s]
            [clojure.string :as string]
            [clojure.string :as str]))

(def Unit (s/enum -1 0 1))

(def DirLoc
  {:loc [s/Num s/Num]
   :dir [Unit Unit]})

(s/defn signum :- s/Int
  [n :- s/Int]
  (cond
    (pos? n) 1
    (zero? n) 0
    :else -1))

(s/defn step :- DirLoc
  [dirloc :- DirLoc
   step :- s/Num]
  (let [[dx dy] (:dir dirloc)
        [x y] (:loc dirloc)
        [movex movey] (if (zero? dx) [(* step dy) 0]
                                     [0 (- (* step dx))])]
    {:loc [(+ x movex) (+ y movey)]
     :dir [(signum movex) (signum movey)]}))

(s/defn parse-step :- s/Int
  [s :- s/Str]
  (let [[dir & dist] s
        dist (clojure.edn/read-string (apply str dist))]
    (* dist (get {\R 1, \L -1} dir 0))))

(s/defn move :- DirLoc
  [s :- s/Str]
  (let [steps (map (comp parse-step str/trim) (string/split s #","))]
    (reduce step
            {:loc [0 0] :dir [0 1]}
            steps)))

(defn d01
  [s]
  (let [[x y] (:loc (move s))]
    (+ (Math/abs x) (Math/abs y))))


(def Dir (s/enum \U \D \R \L))
(def Loc [s/Int s/Int])
(def dirs
  {\U [0 1]
   \D [0 -1]
   \R [1 0]
   \L [-1 0]})

(def keypad
  [[7 4 1]
   [8 5 2]
   [9 6 3]])

(s/defn dir-vector :- Loc
  [dir :- Dir]
  (get dirs dir))

(s/defn in-range :- s/Int
  [n :- s/Int
   limit :- s/Int]
  (min limit (max 0 n)))

(s/defn move-on-keypad :- Loc
  [loc :- Loc
   dir :- Dir]
  (let [size (dec (count keypad))
        [x y] loc
        [dx dy] (dir-vector dir)
        newx (in-range (+ x dx) size)
        newy (in-range (+ y dy) size)]
    [newx newy]))

(s/defn loc-to-key :- s/Str
  [loc :- Loc]
  (str (get-in keypad loc)))

(s/defn follow-instructions :- Loc
  [loc :- Loc
   instructions :- s/Str]
  (reduce move-on-keypad loc instructions))

(s/defn d02 :- s/Str
  [codes :- s/Str]
  (let [lines (str/split codes #"\n")
        locs (reductions follow-instructions [1 1] lines)]
    (apply str (map loc-to-key (rest locs)))))


(s/defn valid-triangle :- s/Bool
  [a :- s/Int
   b :- s/Int
   c :- s/Int]
  (and (< a (+ b c))
       (< b (+ a c))
       (< c (+ a b))))

(s/defn valid-str-triangle :- s/Bool
  [s :- s/Str]
  (apply valid-triangle (map clojure.edn/read-string (str/split (str/trim s) #"\s+"))))

(s/defn d03 :- s/Int
  [s :- s/Str]
  (as-> s x
    (str/split x #"\n")
    (filter valid-str-triangle x)
    (count x)))


(def RoomNumber {:name s/Str, :sector s/Int, :checksum s/Str})

(s/defn parse-room :- RoomNumber
  [s :- s/Str]
  (let [[_ room sector checksum] (re-matches #"([-a-z]+)-([0-9]+)\[([a-z]+)\]" s)]
    {:room room, :sector (read-string sector), :checksum checksum}))

(s/defn real-compare :- Unit
  [a :- MapEntry
   b :- MapEntry]
  (let [freq (compare (val a) (val b))]
    (if (zero? freq)
        (compare (key a) (key b))
        freq)))

(s/defn checksum :- s/Str
  [s :- s/Str])
