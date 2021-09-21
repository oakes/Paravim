(require
  '[clojure.test :as t]
  '[paravim.core-test]
  '[clojure.spec.test.alpha :as st])

(st/instrument)
(t/run-tests 'paravim.core-test)
