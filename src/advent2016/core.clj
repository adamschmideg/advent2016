(ns advent2016.core
  (:use [clojure.edn :only [read-string]]
        [pandect.algo.md5 :only [md5]])
  (:require [schema.core :as s]
            [clojure.string :as string]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.core.matrix :as mat]))

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
    {:name room, :sector (read-string sector), :checksum checksum}))

(s/defn real-compare :- Unit
  [a :- clojure.lang.MapEntry
   b :- clojure.lang.MapEntry]
  (let [freq (compare (val b) (val a))]
    (if (zero? freq)
        (compare (key a) (key b))
        freq)))

(s/defn remove-dashes :- s/Str
  [s :- s/Str]
  (str/replace s "-" ""))

(s/defn checksum :- s/Str
  [s :- s/Str]
  (->> s remove-dashes frequencies (sort real-compare) (take 5) keys (apply str)))

(s/defn real-room? :- s/Bool
  [room :- RoomNumber]
  (= (:checksum room) (checksum (:name room))))

(s/defn d04 :- s/Int
  [s :- s/Str]
  (let [rooms (map parse-room (str/split s #"\n"))]
    (->> rooms
         (filter real-room?)
         (map :sector)
         (apply +))))

(defn d05
  ([s digits]
   (->> (map #(str s %) (range))
       (filter #(str/starts-with? (md5 %) "00000"))
       (map #(subs (md5 %) 5 6))
       (take digits)
       (apply str)))
  ([s] (d05 s 8)))


(s/defn counting :- {s/Any s/Int}
  [counted :- {s/Any s/Int}
   item :- s/Any]
  (let [count (inc (get counted item 0))]
    (assoc counted item count)))

(s/defn counting-seq :- [{s/Any s/Int}]
  [xs :- [[s/Any]]]
  (let [init (repeat (count (first xs)) {})]
    (reduce
      (fn [counts items]
        (map #(counting %1 %2) counts items))
      init
      xs)))

(s/defn most-frequent :- s/Any
  [freqs :- {s/Any s/Int}]
  (key (last (sort-by val freqs))))

(defn d06
  [s]
  (->> (str/split s #"\n")
      counting-seq
      (map most-frequent)
      (apply str)))

(def tls-re #"(.)(.)\2\1")
(def tls-in-brackets-re #"\[[^\]]*(.)(.)\2\1.*]")

(s/defn supports-tls? :- s/Bool
  [ip :- s/Str]
  (when-let [match (re-find tls-re ip)]
    (let [[_ a b] match]
      (and (not (= a b))
           (not (re-find tls-in-brackets-re ip))))))

(defn d07
  [s]
  (->> (str/split s #"\n")
       (filter supports-tls?)
       count))


(def LcdCommand
  {:command (s/enum :rect :rotate-row :rotate-column), :params [s/Int]})

(defn lcd-rect
  [m width height]
  (let [indices (for [x (range width)
                      y (range height)]
                  [y x])]
    (mat/set-indices m indices 1)))

(defn lcd-rotate-row
  [m row count]
  (as-> m $
      (mat/get-row $ row)
      (mat/rotate $ 0 (- count))
      (mat/set-row m row $)))

(defn lcd-rotate-column
  [m column count]
  m)

(s/defn read-lcd-command :- LcdCommand
  [cmd :- s/Str]
  (let [[command [_ & params]]
        (condp #(re-matches %1 %2) cmd
           #"rect (\d+)x(\d+)" :>> #(vector lcd-rect %)
           #"rotate row y=(\d+) by (\d+)" :>> #(vector lcd-rotate-row %)
           #"rotate column x=(\d+) by (\d+)" :>> #(vector lcd-rotate-column %))
        params (map edn/read-string params)]
    {:command command, :params params}))

(defn perform-lcd-command
  [matrix command]
  (let [f (:command command)
        params (:params command)
        all-params (into [matrix] params)]
    (apply f all-params)))
