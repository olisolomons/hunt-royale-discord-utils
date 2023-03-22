(ns loadout-optimisation
  (:require
   [clojure.walk :as walk]))

(def colours [:r :g :b :p :w :y])
(defn ->map
  [nums]
  (->> (map vector
            colours nums)
       (filter (comp pos? second))
       (into {})))

(def map->
  (comp
   (partial map #(or %2 %1) (repeat 0))
   (apply juxt colours)))

(def loadouts
  (map map->
       [{:g 3 :p 1 :b 2}
        #_{:w 1 :r 1 :g 2 :b 2}
        #_{:w 1 :p 1 :g 2 :b 2}
        {:w 1 :p 1 :g 2 :b 1 :r 1}
        {:g 1 :r 1 :p 1 :w 3}]))

(every? (comp #{6} (partial reduce +)) loadouts)

(defn permutations
  [xs]
  (reduce
   #(for [a %1
          b %2]
      (conj a b))
   [[]]
   xs))

(defn extract-subsets
  [l]
  (->> l
       (map (comp range inc))
       (permutations)
       (filter (comp #{3} (partial apply +)))))

(def loadout->choices
  (into {}
        (for [l loadouts]
          {l (set
              (for [s (extract-subsets l)
                    :let[rem (map - l s)]]
                #{rem (apply list s)}))})))

(def by-gear-pieces
  (into (sorted-map)
        (group-by
         #(count (set (mapcat identity %)))
         (permutations (vals loadout->choices)))))

(def answer (first (val (first by-gear-pieces))))

(def unique-pieces
  (distinct (mapcat identity answer)))
(def gems-needed
  (->map (apply map + unique-pieces)))

(map ->map unique-pieces)
;; => ({:g 1, :b 1, :p 1} {:g 2, :b 1} {:r 1, :p 1, :w 1} {:g 1, :w 2})
(zipmap
 (map ->map loadouts)
 (map vec
      (walk/postwalk-replace
       (zipmap unique-pieces (range))
       answer)))
;; => {{:g 3, :b 2, :p 1} [0 1],
;;     {:r 1, :g 2, :b 1, :p 1, :w 1} [1 2],
;;     {:r 1, :g 1, :p 1, :w 3} [3 2]}

gems-needed
;; => {:r 1, :g 4, :b 2, :p 2, :w 3}
;; => {:r 1, :g 5, :b 2, :p 3, :w 4}
