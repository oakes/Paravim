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

(let [*first-run? (atom true)]
  (defn run-only-once [body-fn]
    (when @*first-run?
      (reset! *first-run? false)
      (body-fn))))

(session/merge-into-session
  {;; a custom rule
   :init
   (let [font Font]
     ;; `run-only-once` is a hacky way to
     ;; only change things on init.
     ;; without it, it would be impossible
     ;; to change the font afterwards!
     (run-only-once
       (fn []
         (clarax/merge! font {:size 32}))))
   ;; this overwrites paravim's minimap rule,
   ;; which has the effect of disabling it
   :paravim.session/minimap nil})
