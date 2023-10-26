(ns hunt-royale-discord-utils.expr
  (:require
   [clojure.core.matrix :as matrix]
   [clojure.edn]
   [hunt-royale-discord-utils.nice-parser :refer [nice-parser]]
   [hunt-royale-discord-utils.resources :as res]
   [instaparse.core :as ip]))

(def expr
  #_:clj-kondo/ignore
  (nice-parser
   {:expr       [(<> :space) :-expr (<> :space)]
    (<> :-expr) #{:add :sub :term}
    :add        [:-expr (<> :space) (<> "+") (<> :space) :term]
    :sub        [:-expr (<> :space) (<> "-") (<> :space) :term]
    (<> :term)  #{:stones
                  ;; :resource
                  [(<> "(") (<> :space)
                   :-expr
                   (<> :space) (<> ")")]}

    :stones      [(? :stone-count) (<> "lvl") :stone-lvl]
    :stone-lvl   #"[1-7]"
    :stone-count :nat
    :resource    [:nat (set (map name res/basic-resource-types))]

    :nat   #"[0-9]+"
    :space #"\s*"}
   :expr))

(defn eval-expr
  [tree]
  (ip/transform
   {:nat      clojure.edn/read-string
    :resource (fn [quantity resource-type]
                (matrix/mul
                 (res/->resource (keyword resource-type))
                 quantity))
    :stones   (fn [& value]
                (let [{:keys [stone-count stone-lvl]} (into {} value)]
                  (matrix/mul
                   (res/->resource [:stone
                                    (clojure.edn/read-string
                                     stone-lvl)])
                   (or stone-count 1))))
    :add      matrix/add
    :sub      matrix/sub
    :expr     identity}
   tree))
