(ns hunt-royale-discord-utils.level-plan
  (:require
   [clojure.core.matrix :as matrix]
   [hunt-royale-discord-utils.resources :as res]
   [clojure.string :as str]))

(defn resources->stones
  [resources]
  (map #(matrix/mget resources (res/resource-type->int %))
       res/stone-resource-types))

(defn level-plan
  [from to]
  (rest
   (reductions
    (fn [next-level-crafted [from to]]
      (let [needed (+ (* next-level-crafted 3)
                      (- to from))]
        needed))
    0
    (map vector
         (reverse (resources->stones from))
         (reverse (resources->stones to))))))

(defn level-plan->cost
  [plan]
  (matrix/mmul
   (->> plan
        reverse
        (map max (repeat 0))
        matrix/matrix)
   res/level-costs-matrix))

(defn level-plan->str
  [plan]
  (let [actions
        (->> (map vector
                  (->> plan reverse (map int))
                  res/stone-resource-types
                  (cons "Acquire" (repeat "Make")))
             (filter (comp pos? first))
             (map (fn [[to-make [_stone lvl] word]]
                    (str word " " (int to-make) " lvl" lvl " stone"
                         (when-not (= 1 to-make) "s")))))]
    (if (seq actions)
      (str (str/join "\n" actions) "\n"
           "Cost: " (res/pretty-resources (level-plan->cost plan)))
      "Nothing to do!")))
