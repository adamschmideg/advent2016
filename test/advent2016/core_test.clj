(ns advent2016.core-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [advent2016.core :refer :all]))

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
