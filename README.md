To build this project, you'll need the Clojure CLI tool:

https://clojure.org/guides/deps_and_cli


To develop in a browser with live code reloading:

`clj -A:dev:linux dev.clj`

`clj -A:dev:macos dev.clj`

`clj -A:dev:windows dev.clj`


To build a release version for the web:

`clj -A:prod:linux prod.clj`

`clj -A:prod:macos prod.clj`

`clj -A:prod:windows prod.clj`


To develop the native version on each OS:

`clj -A:dev:linux dev.clj native`

`clj -A:dev:macos -J-XstartOnFirstThread dev.clj native`

`clj -A:dev:windows dev.clj native`


To build the native version as a jar file:

`clj -A:prod prod.clj uberjar`
