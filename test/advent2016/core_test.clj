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
