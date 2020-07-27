(ns paravim.vim
  (:require [paravim.core :as c]
            [paravim.session :as session]
            [paravim.constants :as constants]
            [paravim.utils :as utils]
            [paravim.buffers :as buffers]
            [odoyle.rules :as o]
            [libvim-clj.core :as v]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [parinferish.core :as par])
  (:import [java.time LocalDate]))

(def ^:dynamic *update-ui?* true)
(def ^:dynamic *update-window?* true)

(defn set-window-size! [vim session width height]
  (let [{:keys [font-height font-width] :as constants} (session/get-constants session)
        current-tab (:id (session/get-current-tab session))]
    (when-let [{:keys [top bottom]} (session/get-text-box session current-tab)]
      (let [font-size-multiplier (:multiplier (session/get-font session))
            text-height (- (bottom height font-size-multiplier)
                           (top height font-size-multiplier))
            font-height (* font-height font-size-multiplier)
            font-width (* font-width font-size-multiplier)]
        (doto vim
          (v/set-window-width (/ width font-width))
          (v/set-window-height (/ (max 0 text-height) font-height)))))))

(defn update-window-size! [{:keys [:paravim.start/vim] :as game}]
  (let [width (utils/get-width game)
        height (utils/get-height game)]
    (set-window-size! vim @session/*session width height))
  nil)

(defn ready-to-append? [session vim output]
  (and (seq output)
       (= 'NORMAL (v/get-mode vim))
       (not= ::constants/repl-out (:id (session/get-current-tab session)))))

(defn apply-parinfer! [session vim]
  (let [current-buffer (:buffer-id (session/get-current-buffer session))
        {:keys [parsed-code needs-parinfer?]} (session/get-buffer session current-buffer)]
    (when needs-parinfer?
      (let [cursor-line (v/get-cursor-line vim)
            cursor-column (v/get-cursor-column vim)
            diffs (par/diff parsed-code)]
        (when (seq diffs)
          (v/input vim "i")
          (doseq [{:keys [line column content action]} diffs]
            ;; move all the way to the first column
            (dotimes [_ (v/get-cursor-column vim)]
              (v/input vim "<Left>"))
            ;; move to the right line
            (let [line-diff (- line (dec (v/get-cursor-line vim)))]
              (cond
                (pos? line-diff)
                (dotimes [_ line-diff]
                  (v/input vim "<Down>"))
                (neg? line-diff)
                (dotimes [_ (* -1 line-diff)]
                  (v/input vim "<Up>"))))
            ;; move to the right column
            (dotimes [_ column]
              (v/input vim "<Right>"))
            ;; delete or insert characters
            (doseq [ch (seq content)]
              (case action
                :remove
                (v/input vim "<Del>")
                :insert
                (v/input vim (str ch)))))
          ;; go back to the original position
          (v/set-cursor-position vim cursor-line cursor-column)
          (v/input vim "<Esc>")))
      (swap! session/*session
             (fn [m]
               (-> m
                   (session/insert current-buffer :needs-parinfer? false)
                   (c/insert-buffer-refresh current-buffer)))))))

(defn read-text-resource [path]
  (-> path io/resource slurp str/split-lines))

(def current-year (.getYear (LocalDate/now)))
(def ascii-art {"smile" nil
                "intro" nil
                "cat" (LocalDate/of current-year 8 8)
                "usa" (LocalDate/of current-year 7 4)
                "christmas" (LocalDate/of current-year 12 25)})

(defn assoc-ascii [session constants ascii-name]
  (c/new-tab! (:command-chan (session/get-globals session)) session ::constants/files)
  (let [ascii-key (keyword "paravim.session" ascii-name)]
    (-> session
        (session/insert ascii-key (buffers/->ascii ascii-key constants (read-text-resource (str "ascii/" ascii-name ".txt"))))
        (o/insert ::session/vim ::session/ascii ascii-key))))

(defn dissoc-ascii [session ascii-name]
  (-> session
      (c/remove-buffer ascii-name)
      (o/insert ::session/vim ::session/ascii false)))

(defn change-ascii [session constants s]
  (let [{:keys [mode ascii command-text command-start]} (session/get-vim session)]
    (cond
      (and ascii *update-ui?*)
      (dissoc-ascii session ascii)
      (and (= mode 'COMMAND_LINE)
           (= s "<Enter>")
           (= command-start ":")
           (contains? ascii-art command-text))
      (assoc-ascii session constants command-text)
      :else
      session)))

(defn init-ascii! []
  (let [now (LocalDate/now)]
    (when-let [ascii (or (some (fn [[ascii date]]
                                 (when (= now date)
                                   ascii))
                               ascii-art)
                         "intro")]
      (swap! session/*session
             (fn [session]
               (assoc-ascii session (session/get-constants session) ascii))))))

(defn input [vim session s]
  (if (= 'COMMAND_LINE (v/get-mode vim))
    (let [{:keys [command-text command-start command-completion]} (session/get-vim session)
          pos (v/get-command-position vim)]
      (case s
        "<Tab>"
        (when (and (= (count command-text) pos) command-completion)
          (let [first-part (or (some-> command-text (str/index-of " ") inc)
                               0)]
            (dotimes [_ (- (count command-text) first-part)]
              (v/input vim "<BS>"))
            (doseq [ch command-completion]
              (v/input vim (str ch)))))
        ("<Right>" "<Left>" "<Up>" "<Down>")
        nil
        "<Enter>"
        (if (and (= ":" command-start)
                 (contains? ascii-art command-text))
          (v/input vim "<Esc>")
          (v/input vim s))
        (if (and (= s "!")
                 (not (seq (some-> command-text str/trim))))
          nil ;; disable shell commands for now
          (v/input vim s))))
    (v/input vim s)))

(defn update-buffer [session buffer-ptr cursor-line cursor-column]
  (if-let [buffer (session/get-buffer session buffer-ptr)]
    (-> session
        (session/insert buffer-ptr (assoc buffer :cursor-line cursor-line :cursor-column cursor-column))
        (c/insert-buffer-refresh buffer-ptr))
    session))

(defn update-cursor-position! [vim cursor-line cursor-column]
  (v/set-cursor-position vim (inc cursor-line) cursor-column)
  (swap! session/*session update-buffer (v/get-current-buffer vim) cursor-line cursor-column)
  nil)

(defn update-after-input [session vim s]
  (let [;; get state from vim
        current-buffer (v/get-current-buffer vim)
        mode (v/get-mode vim)
        cursor-line (dec (v/get-cursor-line vim))
        cursor-column (v/get-cursor-column vim)
        ;; update ascii if necessary
        constants (session/get-constants session)
        session (if s
                 (change-ascii session constants s)
                 session)
        ;; get vim from session
        old-vim-info (session/get-vim session)
        old-mode (:mode old-vim-info)
        ;; update vim record
        vim-info {::session/mode mode
                  ::session/command-start (:command-start old-vim-info)
                  ::session/command-text (:command-text old-vim-info)
                  ::session/command-completion (:command-completion old-vim-info)
                  ::session/message nil ;; clear any pre-existing message
                  ::session/visual-range (when (v/visual-active? vim)
                                           (-> (v/get-visual-range vim)
                                               (update :start-line dec)
                                               (update :end-line dec)
                                               (assoc :type (v/get-visual-type vim))))
                  ::session/highlights (mapv (fn [highlight]
                                               (-> highlight
                                                   (update :start-line dec)
                                                   (update :end-line dec)))
                                         (v/get-search-highlights vim 1 (v/get-line-count vim current-buffer)))}
        vim-info (if (and s (= mode 'COMMAND_LINE))
                   (-> (merge
                         vim-info
                         (c/command-text (v/get-command-text vim) (v/get-command-completion vim))
                         (when (not= old-mode 'COMMAND_LINE)
                           (merge
                             {::session/command-start s}
                             (when (#{"/" "?"} s)
                               {::session/show-search? true}))))
                       (c/assoc-command constants mode (:multiplier (session/get-font session)) (v/get-command-position vim)))
                   (merge vim-info (c/command-text nil nil)))
        session (o/insert session ::session/vim vim-info)]
    (update-buffer session current-buffer cursor-line cursor-column)))

(defn on-input [vim session s]
  (input vim session s)
  (let [session (swap! session/*session update-after-input vim s)
        mode (:mode (session/get-vim session))]
    (when (and (= 'NORMAL mode)
               (not= s "u"))
      (apply-parinfer! session vim))))

(def ^:const max-line-length 100)

(defn on-bulk-input [vim s limit-line-length?]
  (v/execute vim "set paste") ;; prevents auto indent
  (doseq [ch s
          :when (not= ch \return)]
    (when (and limit-line-length?
               (>= (v/get-cursor-column vim) max-line-length))
      (v/input vim "<Enter>"))
    (v/input vim (str ch)))
  (v/execute vim "set nopaste")
  (swap! session/*session update-after-input vim nil))

(defn repl-enter! [vim session {:keys [out out-pipe]}]
  (apply-parinfer! session vim)
  (let [buffer-ptr (v/get-current-buffer vim)
        lines (vec (for [i (range (v/get-line-count vim buffer-ptr))]
                     (v/get-line vim buffer-ptr (inc i))))
        text (str (str/join \newline lines) \newline)]
    (doseq [s ["g" "g" "d" "G"]]
      (on-input vim session s))
    (doto out
      (.write text)
      .flush)
    (doto out-pipe
      (.write text)
      .flush)))

(defn append-to-buffer! [{:keys [:paravim.start/vim] :as game} session input]
  (when-let [buffer (:buffer-id (session/get-tab session ::constants/repl-out))]
    (let [current-buffer (v/get-current-buffer vim)
          cursor-line (v/get-cursor-line vim)
          cursor-column (v/get-cursor-column vim)
          _ (v/set-current-buffer vim buffer)
          line-count (v/get-line-count vim buffer)
          char-count (count (v/get-line vim buffer line-count))]
      (v/set-cursor-position vim line-count (dec char-count))
      (on-input vim session "a")
      (on-bulk-input vim input true)
      (v/set-cursor-position vim (v/get-cursor-line vim) 0) ;; fix the repl's horiz scroll position after printing output
      (on-input vim session "<Esc>")
      (v/set-current-buffer vim current-buffer)
      (v/set-cursor-position vim cursor-line cursor-column))))

(defn on-buf-enter [game vim buffer-ptr]
  (when *update-window?*
    (async/put! (:paravim.start/command-chan game) [:resize-window]))
  (let [path (v/get-file-name vim buffer-ptr)
        lines (vec (for [i (range (v/get-line-count vim buffer-ptr))]
                     (v/get-line vim buffer-ptr (inc i))))]
    (if path
      ;; create or update the buffer
      (let [file (java.io.File. path)
            file-name (.getName file)
            canon-path (.getCanonicalPath file)
            current-tab (or (some
                              (fn [[tab path]]
                                (when (= canon-path (-> path java.io.File. .getCanonicalPath))
                                  tab))
                              constants/tab->path)
                            ::constants/files)
            session @session/*session
            constants (session/get-constants session)
            existing-buffer (session/get-buffer session buffer-ptr)
            buffer (if (and existing-buffer (= (:lines existing-buffer) lines))
                     existing-buffer
                     (assoc (buffers/->buffer buffer-ptr constants path file-name lines current-tab)
                       :cursor-line (dec (v/get-cursor-line vim))
                       :cursor-column (v/get-cursor-column vim)))]
        (when *update-window?*
          (async/put! (:paravim.start/command-chan game) [:set-window-title (str file-name " - Paravim")]))
        (swap! session/*session
          (fn [m]
            (-> m
                (c/update-tab current-tab buffer-ptr)
                (c/update-current-tab current-tab)
                (session/insert buffer-ptr buffer)
                (c/insert-buffer-refresh buffer-ptr)))))
      (do
        (when *update-window?*
          (async/put! (:paravim.start/command-chan game) [:set-window-title "Paravim"]))
        ;; clear the files tab
        (swap! session/*session c/update-tab ::constants/files nil)))))

(defn on-buf-update [game vim buffer-ptr start-line end-line line-count-change]
  (let [first-line (dec start-line)
        last-line (+ (dec end-line) line-count-change)
        lines (vec (for [i (range first-line last-line)]
                     (v/get-line vim buffer-ptr (inc i))))]
    (swap! session/*session c/insert-buffer-update
           {:buffer-id buffer-ptr
            :lines lines
            :first-line first-line
            :line-count-change line-count-change})))

(defn on-buf-delete [buffer-ptr]
  (swap! session/*session c/remove-buffer buffer-ptr))

(defn ->vim []
  (doto (v/->vim)
    v/init
    (v/execute "set hidden")
    (v/execute "set noswapfile")
    (v/execute "set nobackup")
    (v/execute "set nowritebackup")
    (v/execute "set tabstop=2")
    (v/execute "set softtabstop=2")
    (v/execute "set shiftwidth=2")
    (v/execute "set expandtab")
    (v/execute "set hlsearch")
    (v/execute "set fileformats=unix,dos")
    (v/execute "filetype plugin indent on")))

(defn yank-lines [{:keys [start-line start-column end-line end-column]}]
  (let [session @session/*session
        current-buffer (:buffer-id (session/get-current-buffer session))]
    (when-let [{:keys [lines]} (session/get-buffer session current-buffer)]
      (let [yanked-lines (subvec lines (dec start-line) end-line)
            end-line (dec (count yanked-lines))
            end-column (cond-> end-column
                               (and (pos? end-column)
                                    (< end-column (count (nth yanked-lines end-line))))
                               inc)]
        (-> yanked-lines
            (update end-line subs 0 end-column)
            (update 0 subs start-column))))))

(defn init [{:keys [:paravim.start/vim] :as game}]
  (v/set-on-quit vim (fn [buffer-ptr force?]
                       (System/exit 0)))
  (v/set-on-auto-command vim (fn [buffer-ptr event]
                               (case event
                                 EVENT_BUFENTER
                                 (when *update-ui?*
                                   (on-buf-enter game vim buffer-ptr))
                                 EVENT_BUFDELETE
                                 (on-buf-delete buffer-ptr)
                                 nil)))
  (v/set-on-buffer-update vim (partial on-buf-update game vim))
  (v/set-on-stop-search-highlight vim (fn []
                                        (async/put! (:paravim.start/command-chan game) [:update-vim {::session/show-search? false}])))
  (v/set-on-unhandled-escape vim (fn []
                                   (async/put! (:paravim.start/command-chan game) [:update-vim {::session/show-search? false}])))
  (v/set-on-yank vim (fn [yank-info]
                       (try
                         (when-let [lines (yank-lines yank-info)]
                           (async/put! (:paravim.start/command-chan game) [:set-clipboard-string (str/join \newline lines)]))
                         (catch Exception e (.printStackTrace e)))))
  (v/set-on-message vim (fn [{:keys [message]}]
                          (async/put! (:paravim.start/command-chan game) [:update-vim {::session/message message}])))
  (binding [*update-window?* false]
    (run! #(v/open-buffer vim (constants/tab->path %)) [::constants/repl-in ::constants/repl-out ::constants/files]))
  (init-ascii!)
  (update-window-size! game))

