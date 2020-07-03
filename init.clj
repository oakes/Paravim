;; this is an example of an init file for paravim.
;; paravim will load it during init if you set its
;; path in the paravim.init system property.
;; you can do that with the clojure CLI tool like this:
;;
;; :jvm-opts ["-Dparavim.init=init.clj"]

(require
  '[paravim.session :as session]
  '[clarax.rules :as clarax])

(import
  '[paravim.session Init Font])

(session/merge-into-session
  {:init
   (let [init Init
         font Font]
     (clarax/merge! font {:size 32}))})
