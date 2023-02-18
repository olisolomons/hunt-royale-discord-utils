(ns hunt-royale-discord-utils.core
  (:gen-class)
  (:require
   [instaparse.core :as ip]
   [clojure.core.matrix :as matrix]
   [hunt-royale-discord-utils.nice-parser :refer [nice-parser]]
   [hunt-royale-discord-utils.resources :as res]
   [ring-discord-auth.ring :refer [wrap-authenticate]]
   [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
   [ring.util.response :refer [response]]
   [ring.adapter.jetty :refer [run-jetty]]))

(def expr
  #_:clj-kondo/ignore
  (nice-parser
   {:expr       :-expr
    (<> :-expr) #{:add :sub :term}
    :add        [:-expr (<> :space) (<> "+") (<> :space) :term]
    :sub        [:-expr (<> :space) (<> "-") (<> :space) :term]
    :fn         [#{"cost"} (<> :space)
                 (<> "(") (<> :space)
                 :-expr
                 (<> :space) (<> ")")]
    (<> :term)  #{:stones
                  :resource
                  :fn
                  [(<> "(") (<> :space)
                   :-expr
                   (<> :space) (<> ")")]}

    :stones      [(? :stone-count) (<> "lvl") :stone-lvl]
    :stone-lvl   #"[1-6]"
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
    :fn       (fn [fn-name arg]
                (case fn-name
                  "cost"
                  (matrix/mmul arg res/cumulative-level-costs-matrix)))
    :expr     identity}
   tree))

(def app-id (System/getenv "APP_ID"))
(def public-key (System/getenv "PUBLIC_KEY"))

(defn handler [{{:keys [type]} :body :as _request}]
  (response
   (case type
     1 {:type 1} ; Respond to PING with PONG
     2 {:type 4 :data {:content "Hello!"}}
     3 {:type 6}))) ; ACK component presses but do nothing further

(defn start-server
  []
  (run-jetty
   (-> handler
       wrap-json-response
       (wrap-json-body {:keywords? true})
       (wrap-authenticate public-key))
   {:port 8080}))

(defn register
  []
  (println "registering"))

(defn -main [& args]
  (case args
    nil          (start-server)
    ("register") (register)
    (binding [*out* *err*]
      (println "Invalid args: " (str args))
      (System/exit 1))))

(comment
  (res/pretty-resources (eval-expr (expr "cost(4lvl6)-cost(500lvl1)")))

  )
