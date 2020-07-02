(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown task:" task-name)
  (System/exit 1))

(require '[paravim.start-dev])

(defmethod task "native"
  [_]
  (paravim.start-dev/start))

(task *command-line-args*)
