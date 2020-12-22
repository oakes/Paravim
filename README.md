<p align="center">
  <img src="screenshot.png" width="510" >
</p>

## Introduction

Paravim is an editor for Clojure that...
* Runs *inside* your project as a library, not as a separate application like traditional editors
* Uses a [real copy of Vim](https://github.com/oakes/libvim-clj), not some kind of emulation
* Renders its UI [with OpenGL](https://github.com/oakes/play-cljc), not with a forked web browser

See [the website](https://sekao.net/paravim/) for more info and join the discussion on [/r/Paravim](http://www.reddit.com/r/Paravim).

## Getting Started

The easiest way to run it is with the [Clojure CLI tool](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools). On Windows or Linux, run this in any directory you want:

```
clojure -Sdeps "{:deps {paravim/paravim {:mvn/version \""RELEASE\""}}}" -m paravim.start
```

On Mac OS, you need to add a special flag:

```
clojure -Sdeps "{:deps {paravim/paravim {:mvn/version \""RELEASE\""}}}" -J-XstartOnFirstThread -m paravim.start
```

For Leiningen users, see [this sample project.clj](https://gist.github.com/oakes/d85d6f9013d063b07896ffd8f6733a19).

NOTE: On Linux, there have been some `UnsatisfiedLinkError`s due to `libtinfo.so.5` not being present. If you have apt, try `apt install libtinfo5`. If you use Arch btw, see [this issue](https://github.com/oakes/Paravim/issues/5) for a solution.

## Development

* To develop: `clj -M:dev native`
  * On Mac OS, you'll need to run `clj -M:dev:macos native`
* To run the tests: `clj -M:test`
  * On Mac OS, you'll need to run `clj -M:test:macos`
* To install the release version: `clj -M:prod install`

## Licensing

All files that originate from this project are dedicated to the public domain. I would love pull requests, and will assume that they are also dedicated to the public domain.
