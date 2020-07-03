;; this is an example of an init file for paravim.
;; paravim will load it during init if you set its
;; path in the paravim.init system property.
;; you can do that with the clojure CLI tool like this:
;;
;; :jvm-opts ["-Dparavim.init=init.clj"]

(require
  '[paravim.session :as session]
  '[clara.rules :as clara]
  '[clarax.rules :as clarax])

(import '[paravim.session Font])

(defrecord Init [])

(session/merge-into-session
  {;; this is a new custom rule
   ;; that will run when the session initializes
   :init
   (let [init Init
         font Font]
     (clara/retract! init) ;; ensures this rule doesn't run again
     (clarax/merge! font {:size 32}))
   ;; this overwrites paravim's minimap rule,
   ;; which has the effect of disabling it
   :paravim.session/minimap nil})

(swap! session/*session clara/insert (->Init)) ;; make the :init rule run
