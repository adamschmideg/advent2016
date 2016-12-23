(ns advent2016.core-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [advent2016.core :refer :all]
            [clojure.core.matrix :as mat]))

(facts "d01"
  (facts "signum"
         (signum -5) => -1
         (signum 0) => 0
         (signum 8) => 1)
  (facts "parse-step"
         (parse-step "R3") => 3
         (parse-step "L12") => -12
         (parse-step "X55") => 0)
  (tabular
    (fact "step loc"
      (:loc (step {:dir ?dir :loc [0 0]} ?step)) => ?new-loc)
    ?dir ?step ?new-loc
    [0 1] 2 [2 0]
    [1 0] 3 [0 -3]
    [0 -1] 4 [-4 0]
    [-1 0] 5 [0 5])
  (tabular
    (fact "move"
       (:loc (move ?moves)) => ?loc)
    ?moves ?loc
    "R5" [5 0]
    "L4" [-4 0]
    "R1,R1" [1 -1])

  (tabular
    (fact "d01"
          (d01 ?path) => ?distance)
    ?path ?distance
    "R2, L3" 5
    "R2, R2, R2" 2
    "R5, L5, R5, R3" 12))


(facts "d02"
  (facts "in-range"
    (fact (in-range 3 2) => 2)
    (fact (in-range -8 3) => 0))
  (facts "move-on-keypad"
    (fact (move-on-keypad [1 2] \D) => [1 1])
    (fact (move-on-keypad [1 2] \U) => [1 2]))
  (facts "follow-instructions"
    (fact (follow-instructions [0 0] "UUU") => [0 2]))
  (facts "d02"
    (fact (d02 "ULL\nRRDDD\nLURDL\nUUUUD") => "1985")))


(facts "d03"
  (tabular "valid-triangle"
    (fact (valid-triangle ?a ?b ?c) => ?valid)
    ?a ?b ?c ?valid
     5  5  5   true
     5 10 25  false
    10 25  5  false
    25  5 10  false)
  (fact "valid-str-triangle" (valid-str-triangle "5 10 25") => false)
  (fact "d03"
    (d03 " 5  5  5\n5 10 25\n10 25 5\n") => 1))


(facts "d04"
  (fact "parse-room"
    (parse-room "abc-de-f-567[decoy]") => {:name "abc-de-f", :sector 567, :checksum "decoy"})
  (tabular
    (fact "real-compare" (real-compare (first ?a) (first ?b)) => ?compared)
    ?a ?b ?compared
    {:z 1} {:a 2} 1
    {:x 3} {:y 3} -1
    {:q 7} {:q 7} 0)
  (facts "checksum"
    (fact "most frequent" (checksum "zzzzzz-fffff-xxxx-bbb-yy-a") => "zfxby")
    (fact "alphabetic" (checksum "aaaaa-bbb-z-y-x") => "abxyz"))
  (fact "d04"
    (d04 "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]") => 1514))

(facts "d05"
  (fact "2" (d05 "abc" 2) => "18"))

(facts "d06"
  (facts "counting"
    (fact "new item" (counting {:a 2} :b) => {:a 2, :b 1})
    (fact "existing item" (counting {:c 3, :d 4} :c) => {:c 4, :d 4}))
  (fact "counting-seq"
    (counting-seq ["ab" "aa"]) => [{\a 2}, {\a 1 \b 1}])
  (fact "most-frequest" (most-frequent {:a 3, :b 8, :c 1}) => :b)
  (fact "d06"
    (d06 "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar") => "easter"))

(facts "d07"
  (tabular "supports-tls?"
    (fact (supports-tls? ?ip) => ?supports)
    ?ip ?supports
    "abba[mnop]qrst" truthy
    "abcd[bddb]xyyx" falsey
    "aaaa[qwer]tyui" falsey
    "ioxxoj[asdfgh]zxcvbn" truthy)
  (fact "d07"
    (d07 "XXX\nabba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn") => 2))

(facts "d08"
  (tabular "read-lcd-command"
    (fact (read-lcd-command ?command-str) => ?command)
    ?command-str ?command
    "rect 3x4" {:command lcd-rect, :params [3 4]}
    "rotate column x=1 by 2" {:command lcd-rotate-column, :params [1 2]}
    "rotate row y=0 by 4" {:command lcd-rotate-row, :params [0 4]})
  (fact "lcd-rect" (lcd-rect [[0 0 0] [0 0 0]] 2 1) => [[1 1 0] [0 0 0]])
  (fact "lcd-rotate-row" (lcd-rotate-row [[1 2 3] [4 5 6]] 0 1) => [[3 1 2] [4 5 6]])
  (fact "lcd-rotate-column" (lcd-rotate-column [[1 2 3] [4 5 6]] 0 1) => [[4 2 3] [1 5 6]])
  (tabular "commands"
    (let [m  [[0 1 0]
              [1 0 0]]]
      (fact (perform-lcd-command m ?command) => ?result))
    ?command ?result
    {:command lcd-rect, :params [3 1]} [[1 1 1]
                                        [1 0 0]]
    {:command lcd-rotate-row, :params [0 2]} [[1 0 0]
                                              [1 0 0]]
    {:command lcd-rotate-row, :params [1 2]} [[0 1 0]
                                              [0 0 1]]))

(facts "d09"
  (tabular "decompress-one"
    (fact (decompress-one ?raw) => ?decompressed)
    ?raw ?decompressed
    "ADVENT" ["ADVENT" ""]
    "A(1x5)BC" ["ABBBBB" "C"]
    "(3x3)XYZ" ["XYZXYZXYZ" ""]
    "A(2x2)BCD" ["ABCBC" "D"]
    "(6x1)(1x3)A" ["(1x3)A" ""])
  (fact "d09" (d09 "A(2x2)BCD(2x2)EFG") => "ABCBCDEFEFG"))


(facts "d10"
  (fact "bot-ready" (bot-ready {0 {:chips []}, 2 {:chips [2 3]}, 1 {:chips [1 4]}}) => {:chips [1 4]}))

