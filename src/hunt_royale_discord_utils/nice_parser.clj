(ns hunt-royale-discord-utils.nice-parser
  (:require
   [instaparse.core :as ip]
   [instaparse.combinators :as ipc]))

(def nice-parser-shorthands
  {'?  #'ipc/opt
   '<> #'ipc/hide
   '+  #'ipc/plus
   '*  #'ipc/star
   '/  #'ipc/ord
   '&  #'ipc/look
   '!  #'ipc/neg})

(defn transform-rule
  [rule]
  (cond
    (instance? clojure.lang.Keyword rule)
    `(ipc/nt ~rule)

    (list? rule)
    (if-let [combinator (nice-parser-shorthands (first rule))]
      (cons (symbol combinator)
            (apply list
                   (map transform-rule (rest rule))))
      `(eval (transform-rule ~rule)))

    (sequential? rule)
    `(ipc/cat
      ~@(map transform-rule rule))

    (set? rule)
    `(ipc/alt
      ~@(map transform-rule rule))

    (instance? java.util.regex.Pattern rule)
    `(ipc/regexp ~rule)

    (string? rule)
    `(ipc/string ~rule)))

(defmacro nice-parser
  [grammar start]
  `(ip/parser
    ~(into
      {}
      (map
       (fn [[rule-name definition]]
         (if (and (list? rule-name)
                  (= (count rule-name) 2)
                  (= (first rule-name) '<>))
           [(second rule-name) `(ipc/hide-tag ~(transform-rule definition))]
           [rule-name (transform-rule definition)]))
       grammar))
    :start ~start))
