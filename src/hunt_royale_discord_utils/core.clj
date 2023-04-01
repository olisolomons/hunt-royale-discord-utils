(ns hunt-royale-discord-utils.core
  (:gen-class)
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [discljord.formatting :as formatting]
   [discljord.messaging :as msg]
   [hunt-royale-discord-utils.expr :as expr]
   [hunt-royale-discord-utils.gear :as gear]
   [hunt-royale-discord-utils.level-plan :as level-plan]
   [hunt-royale-discord-utils.resources :as res]
   [instaparse.core :as ip]
   [juxt.clip.core :as clip]
   [ring-discord-auth.ring :refer [wrap-authenticate]]
   [ring.adapter.jetty :refer [run-jetty]]
   [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
   [ring.util.response :refer [response]]))

(defmulti handle-slash-command (comp keyword :name))

(defmethod handle-slash-command :loadouts
  [{[{loadouts :value}] :options} _]
  {:content
   (formatting/code-block
    (let [parsed
          (-> loadouts
              gear/loadouts
              gear/parsed->map)]
      (if (ip/failure? parsed)
        (print-str parsed)
        (if-let [answer (->> parsed
                          (map gear/map->)
                          gear/optimise-loadouts)]
          (let [unique-pieces
                (distinct (mapcat identity answer))
                gems-needed
                (gear/->map (apply map + (map :stones unique-pieces)))
                loadouts-indexed
                (zipmap
                 (map gear/map->string parsed)
                 (map vec
                      (walk/postwalk-replace
                       (zipmap unique-pieces (rest (range)))
                       answer)))]
            (str "Input: " loadouts
                 "\n\n"
                 "Gems needed: " (gear/map->string gems-needed)
                 "\n"
                 "All weapons:\n"
                 (str/join
                  "\n"
                  (map-indexed
                   (fn [index {:keys [type stones]}]
                     (str "(" (inc index) ") "
                          (name type) ": "
                          (gear/map->string (gear/->map stones))))
                   unique-pieces))
                 "\n\n"
                 "Loadouts:\n"
                 (str/join
                  "\n"
                  (map
                   (fn [[loadout [piece-a piece-b]]]
                     (str " - Loadout \"" loadout
                          "\" uses gear pieces ("
                          piece-a
                          ") and (" piece-b ")"))
                   loadouts-indexed))))
          (str "Input: " loadouts
               "\nOnly 6 weapon slots are currently supported")))))})

(defmethod handle-slash-command :calc
  [{[{:keys [value]}] :options} _]
  {:content
   (formatting/code-block
    (let [result (-> value
                     expr/expr
                     expr/eval-expr
                     res/pretty-resources)]
      (if (ip/failure? result)
        (print-str result)
        (str value "\n= " result))))})

(defmethod handle-slash-command :level-plan
  [{[{from-str :value} {to-str :value}] :options} _]
  {:content
   (formatting/code-block
    (let [from (-> from-str
                   expr/expr
                   expr/eval-expr)
          to (-> to-str
                 expr/expr
                 expr/eval-expr)]
      (cond
        (ip/failure? from)
        (print-str from)

        (ip/failure? to)
        (print-str to)

        :else
        (str
         "from: " from-str "\n"
         "to: " to-str "\n"
         (level-plan/level-plan->str
          (level-plan/level-plan from to))))))})

(defmethod handle-slash-command :trade
  [{[{hunter :value}] :options}
   {{:keys [member channel_id]} :body
    discord                     :discord}]
  (->> (msg/get-channel-messages! discord channel_id :limit 100)
       deref
       (filter (comp #{"trade"} :name :interaction))
       (filter (comp #{(-> member :user :id)} :id :user :interaction))
       (map
        (fn [message] @(msg/delete-message! discord channel_id (:id message))))
       doall)
  (if (seq hunter)
    {:content
     (str (formatting/mention-user (:user member))
          " is looking for "
          hunter " pieces!")}
    {:content (str "Your previous trade has been removed! "
                   "(If there was one, anyway...)")
     ;; ephemeral
     :flags (bit-shift-left 1 6)}))

(defn handler
  [{{:keys [type data] :as _body} :body :as request}]
  (response
   (case type
     1 {:type 1} ; Respond to PING with PONG
     2 {:type 4
        :data
        (handle-slash-command data request)}
     3 {:type 6}))) ; ACK component presses but do nothing further

(defn wrap-discord [handler discord]
  (fn [request] (handler (assoc request :discord discord))))

(defn start-server
  [port discord public-key]
  (run-jetty
   (-> handler
       (wrap-discord discord)
       wrap-json-response
       (wrap-json-body {:keywords? true})
       (wrap-authenticate public-key))
   {:port port :join? false}))

(defn stop-server
  [server]
  (.stop server)
  (.join server))

(defn get-env
  ([name]
   (get-env name nil))
  ([name default]
   (or (System/getenv name) (System/getProperty name) default)))

(def system-config
  {:components
   {:token      {:start `(get-env "TOKEN")}
    :app-id     {:start `(get-env "APP_ID")}
    :public-key {:start `(get-env "PUBLIC_KEY")}
    :port       {:start #(Integer/parseInt (get-env "PORT" "8080"))}
    :discord    {:start `(msg/start-connection! ~(clip/ref :token))
                 :stop  `msg/stop-connection!}
    :server     {:start `(start-server ~(clip/ref :port)
                                       ~(clip/ref :discord)
                                       ~(clip/ref :public-key))
                 :stop  `stop-server}}})

(defn register-commands!
  [{:keys [discord app-id]}]
  @(msg/bulk-overwrite-global-application-commands!
    discord app-id
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
                     :required    true}]}
     {:name        "loadouts"
      :description "Find an optimal gem configuration for sharing weapons between loadouts in maze gear."
      :options     [{:type        3
                     :name        "loadouts"
                     :description "A comma separated list of loadouts, where each loadout is the 6 gems on your weapons, e.g. \"6g,3b3g\""
                     :required    true}]}]))

(defn -main [& _]
  ;; start
  (clip/start system-config)
  ;; block forever
  @(promise))

(comment
  (require '[juxt.clip.repl :as clip-repl])

  (register-commands! clip-repl/system)
  (def discord (:discord clip-repl/system))

  )
