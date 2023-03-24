(ns hunt-royale-discord-utils.gear
  (:require
   [hunt-royale-discord-utils.nice-parser :refer [nice-parser]]
   [instaparse.core :as ip]
   [clojure.string :as str]))

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

(defn cartesian-product
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
       cartesian-product
       (filter (comp #{3} (partial apply +)))))

(defn loadout->choices
  [loadout]
  (set
   (for [subset (extract-subsets loadout)
         :let
         [remaining (map - loadout subset)]]
     #{{:type :sword :stones remaining}
       {:type :ring :stones (seq subset)}})))


(defn optimise-loadouts
  [loadouts]
  (when (every? (comp #{6} (partial apply +)) loadouts)
    (->> loadouts
         (map loadout->choices)
         cartesian-product
         (apply min-key #(count (distinct (mapcat identity %)))))))

(def loadouts
  #_:clj-kondo/ignore
  (nice-parser
   {:loadouts    [:loadout (* [(<> ",") :loadout])]
    :loadout     [(<> #"\s*") (+ [:stone (<> #"\s*")])]
    :stone       [(? :stone-count) :colour]
    :stone-count :nat
    :colour      (set (map name colours))
    :nat         #"\d+"}
   :loadouts))

(defn parsed->map
  [tree]
  (ip/transform
   {:nat clojure.edn/read-string
    :stone (fn [& value]
             (let [{:keys [stone-count colour]} (into {} value)]
               {(keyword colour) (or stone-count 1)}))
    :loadout (partial merge-with +)
    :loadouts list}
   tree))

(defn map->string
  [m]
  (->> m
       (map (fn [[colour num]]
              (str (when (> num 1) num) (name colour))))
       (str/join " ")))
