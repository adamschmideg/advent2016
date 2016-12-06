(ns advent2016.core
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
