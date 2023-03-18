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
   [ring.adapter.jetty :refer [run-jetty]]
   [discljord.messaging :as msg]
   [discljord.connections :as conn]
   [discljord.formatting :as formatting]
   [clojure.core.async :as a]
   [clojure.string :as str]))

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

(defn resources->stones
  [resources]
  (map #(matrix/mget resources (res/resource-type->int %))
       res/stone-resource-types))

(defn level-plan
  [from to]
  (reductions
   (fn [next-level-crafted [from to]]
     (let [needed (+ (* next-level-crafted 3)
                     (- to from))]
       needed))
   0
   (map vector
        (reverse (resources->stones from))
        (reverse (resources->stones to)))))

(defn level-plan->str
  [plan]
  (let [actions
        (->> (map vector
                  (->> plan rest reverse (map int))
                  res/stone-resource-types
                  (cons "Acquire" (repeat "Make")))
             (filter (comp pos? first))
             (map (fn [[to-make [_stone lvl] word]]
                    (str word " " (int to-make) " lvl" lvl " stone"
                         (when-not (= 1 to-make) "s")))))]
    (if (seq actions)
      (str/join "\n" actions)
      "Nothing to do!")))

(comment
  (level-plan->str
   (level-plan (eval-expr (expr "74lvl1+10lvl3+2lvl5"))
               (eval-expr (expr "lvl5"))))
  (level-plan->str
   (level-plan (eval-expr (expr "369lvl2+16lvl3"))
               (eval-expr (expr "11lvl5"))))
  (res/pretty-resources (eval-expr (expr "cost(4lvl6)-cost(500lvl1)")))
  )

(def token (System/getenv "TOKEN"))
(def app-id (System/getenv "APP_ID"))
(def public-key (System/getenv "PUBLIC_KEY"))

(defmulti handle-slash-command (comp keyword :name))

(defmethod handle-slash-command :calc
  [{[{:keys [value]}] :options} _]
  (formatting/code-block
   (let [result (-> value
                    expr
                    eval-expr
                    res/pretty-resources)]
     (if (ip/failure? result)
       (print-str result)
       (str value "\n= " result)))))

(defmethod handle-slash-command :level-plan
  [{[{from-str :value} {to-str :value}] :options} _]
  (formatting/code-block
   (let [from (-> from-str
                  expr
                  eval-expr)
         to (-> to-str
                expr
                eval-expr)]
     (cond
       (ip/failure? from)
       (print-str from)

       (ip/failure? to)
       (print-str to)

       :else
       (str
        "from: " from-str "\n"
        "to: " to-str "\n"
        (level-plan->str
         (level-plan from to)))))))

(defmethod handle-slash-command :trade
  [{[{hunter :value}] :options} {:keys [member channel_id]}]
  (str (formatting/mention-user (:user member)) " is looking for " hunter " pieces!"))

(defn handler
  [{{:keys [type data] :as body} :body :as _request}]
  (response
   (case type
     1 {:type 1} ; Respond to PING with PONG
     2 {:type 4
        :data
        {:content
         (handle-slash-command data body)}}
     3 {:type 6}))) ; ACK component presses but do nothing further

(defn start-server
  []
  (run-jetty
   (-> handler
       wrap-json-response
       (wrap-json-body {:keywords? true})
       (wrap-authenticate public-key))
   {:port 8080}))

(defn connection!
  []
  (let [events (a/chan 100 (comp (filter (comp #{:interaction-create} first)) (map second)))]
    {:api    (msg/start-connection! token)
     :events events
     :conn   (conn/connect-bot! token events :intents #{})}))

(defn register-commands!
  [{:keys [api]}]
  @(msg/bulk-overwrite-global-application-commands!
    api app-id
    [{:name        "calc"
      :description "Evaluate a stone/resources/costs expression"
      :options     [{:type        3
                     :name        "expression"
                     :description "The expression to evaluate"
                     :required    true}]}
     {:name        "level-plan"
      :description "Tell you how many stones of each type to combine to get you where you want to be."
      :options     [{:type        3
                     :name        "from"
                     :description "Your current stones"
                     :required    true}
                    {:type        3
                     :name        "to"
                     :description "You target stones"
                     :required    true}]}
     {:name        "trade"
      :description "Request something you want"
      :options     [{:type        3
                     :name        "hunter"
                     :description "The hunter you want"
                     :required    true}]}]))

(defn -main [& args]
  (start-server))

(comment
  (def api (msg/start-connection! token))
  (register-commands! {:api api})

  (def events (a/chan 100
                      (comp (filter (comp #{:interaction-create} first))
                            (map second))))

  (def conn (conn/connect-bot! token events :intents #{}))

  (def bot-spam "1077511654792773642")
  (def messages @(msg/get-channel-messages! api bot-spam :limit 10))
  (count messages)
  (filter (comp :bot :author) messages)
  )
