(ns DSL-Examples.core)

(def states #{:a :b :c :d})
(def alphabet #{\0 \1})
(defn transition
  [state char]
  (if (= state :a)
    (if (= char \0)
      :a
      :b)
    (if (= state :b)
      (if (= char \0)
        :b
        :d)
      (if (= state :c)
        (if (= char \0)
          :d
          :a)
        (if (= char \0)
          :d
          :d)))))
(def start-state :a)
(def accept-states #{:d})

(defn DFA1
  "Recognizes strings of 1's and 0's with 010 as a substring"
  [input]
  (let [s start-state
        f (reduce #(transition %1 %2) s input)]
    (contains? accept-states f)))

(defn MakeDFA
  "Makes a DFA from the components"
  [states alphabet tran start accepts]
  (fn [input] (let [s start
                    f (reduce #(tran %1 %2) s input)]
                (contains? accepts f))))

(def state-lst [:a :b :c :d])
(def alphabet-lst [\0 \1])
(def matrix [[:b :a]
             [:b :c]
             [:d :a]
             [:d :d]])

(defn MakeTransition
  [states alphabet matrix]
  (let [mapper (fn [x y] {x y})
        inner-maps (map #(reduce merge (map mapper alphabet %)) matrix)
        outer-maps (map mapper states inner-maps)
        lookup (reduce merge outer-maps)]
    (fn [state char]
      (get (get lookup state) char))))

(defn MakeDFA2
  "Create a DFA from it's components"
  [states alphabet matrix start accepts]
  (let [trans (MakeTransition states alphabet matrix)]
    (fn [input]
      (let [s start
                          f (reduce #(trans %1 %2) s input)]
                      (contains? accepts f)))))
