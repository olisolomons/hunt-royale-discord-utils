(ns dev
  (:require
   [clojure.java.io :as io]
   [hunt-royale-discord-utils.core :as hr]
   [juxt.clip.repl :refer [start reset set-init! system]]))

(set-init! (constantly hr/system-config))

(defn load-props
  [file]
  (let [p (java.util.Properties.)]
    (.load p (io/reader file))
    (doseq [[name val] p]
      (System/setProperty name val))))

(load-props ".env")

(comment
  (start)
  (reset)
  system)
