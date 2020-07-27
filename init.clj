;; this is an example of an init file for paravim.
;; paravim will load it during init if you set its
;; path in the paravim.init system property.
;; you can do that with the clojure CLI tool like this:
;;
;; :jvm-opts ["-Dparavim.init=init.clj"]
(ns example.init
  (:require [paravim.session :as session]
            [odoyle.rules :as o]))

(session/reload!
  (fn [session]
    (o/insert session ::session/font ::session/size 24)))
