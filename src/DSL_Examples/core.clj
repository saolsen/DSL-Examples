(ns DSL-Examples.core)

;; So mathematically a DFA is defined as a 5 tuple (1 2 3 4 5)
;; A finite set of states
;; The Alphabet
;; A transition function
;; A start state
;; and a set of accept states.

(def states #{:A :B :C :D})
(def alphabet #{\0 \1})
(defn transition
  [state char]
  (if (= state :A)
    (if (= char \0)
      :A
      :B)
    (if (= state :B)
      (if (= char \0)
        :B
        :C)
      (if (= state :C)
        (if (= char \0)
          :D
          :A)
        (if (= char \0)
          :D
          :D)))))
(def start-state :A)
(def accept-states #{:D})

(defn DFA1
  "Recognizes strings of 1's and 0's with 010 as a substring"
  [input]
  (let [s start-state
        f (reduce #(transition %1 %2) s input)]
    (contains? accept-states f)))
