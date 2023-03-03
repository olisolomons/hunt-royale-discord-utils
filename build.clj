(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'hunt-royale-discord-utils)
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (str "target/" (name lib) "-standalone.jar"))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/compile-clj {:basis     basis
                  :src-dirs  ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file jar-file
           :basis basis
           :main 'hunt-royale-discord-utils.core}))
