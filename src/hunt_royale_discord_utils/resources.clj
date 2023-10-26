(ns hunt-royale-discord-utils.resources
  (:require
   [clojure.core.matrix :as matrix]
   [clojure.string :as str]
   [instaparse.core :as ip]))

(matrix/set-current-implementation :vectorz)

(def basic-resource-types [:gold :maze :dd :kk :yy :epic])
(def stone-resource-types (map vector (repeat :stone) (range 1 (inc 7))))
(def resource-types (concat basic-resource-types
                            stone-resource-types))

(def resource-type->int (zipmap resource-types (range)))

(defn ->resource
  [resource-type]
  (matrix/mset
   (matrix/zero-vector (count resource-types))
   (resource-type->int resource-type)
   1))

(def level-costs
  (into
   {:gold [50 100 400 1000 2000 5000]
    :maze [25 75 250 750 1500 5000]}
   (zipmap [:dd :kk :yy :epic]
           (repeat [5 10 20 50 100 1000]))))

(def level-costs-matrix
  (->> level-costs
       (map (fn [[resource-type costs]]
              (map #(matrix/mul (->resource resource-type) %)
                   (cons 0 costs))))
       (apply map matrix/add)
       matrix/matrix))

(def cumulative-level-costs-matrix
  (let [stone-costs
        (zipmap stone-resource-types
                (reductions
                 (fn [acc cost] (matrix/add cost (matrix/mul 3 acc)))
                 (->resource [:stone 1])
                 (rest level-costs-matrix)))
        costs
        (into stone-costs
              (map (juxt identity ->resource)
                   basic-resource-types))]
    (matrix/matrix
     (map costs resource-types))))

(defn pretty-resources
  [resources]
  (if (ip/failure? resources)
    resources
    (str/replace
     (->> (map (fn [quantity resource-type]
                 (when-not (zero? quantity)
                   (str (int quantity)
                        (if (keyword? resource-type)
                          (name resource-type)
                          (str "lvl" (second resource-type))))))
               resources
               resource-types)
          (filter some?)
          (str/join "+"))
     "+-"
     "-")))
