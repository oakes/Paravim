(ns paravim.vim
  (:require [paravim.core :as c]
            [libvim-clj.core :as v]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [parinferish.core :as par])
  (:import [java.time LocalDate]))

;; https://vim.fandom.com/wiki/Mapping_keys_in_Vim_-_Tutorial_%28Part_2%29

(def keyword->name
  {:backspace "<BS>"
   :delete "<Del>"
   :tab "<Tab>"
   :enter "<Enter>"
   :escape "<Esc>"
   :up "<Up>"
   :down "<Down>"
   :left "<Left>"
   :right "<Right>"})

(def ^:dynamic *update-ui?* true)

(defn open-buffer-for-tab! [vim {:keys [current-buffer current-tab tab->buffer] :as state}]
  (if-let [buffer-for-tab (tab->buffer current-tab)]
    (when (not= current-buffer buffer-for-tab)
      (v/set-current-buffer vim buffer-for-tab))
    (when-let [path (c/tab->path current-tab)]
      (v/open-buffer vim path))))

(defn set-window-size! [vim {:keys [font-height font-width font-size-multiplier text-boxes current-tab] :as state} width height]
  (when-let [{:keys [top bottom]} (get text-boxes current-tab)]
    (let [text-height (- (bottom height font-size-multiplier)
                         (top height font-size-multiplier))
          font-height (* font-height font-size-multiplier)
          font-width (* font-width font-size-multiplier)]
      (doto vim
        (v/set-window-width (/ width font-width))
        (v/set-window-height (/ (max 0 text-height) font-height))))))

(defn repl-enter! [vim callback {:keys [out out-pipe]}]
  (let [buffer-ptr (v/get-current-buffer vim)
        lines (vec (for [i (range (v/get-line-count vim buffer-ptr))]
                     (v/get-line vim buffer-ptr (inc i))))
        text (str (str/join \newline lines) \newline)]
    (run! callback ["g" "g" "d" "G"])
    (doto out
      (.write text)
      .flush)
    (doto out-pipe
      (.write text)
      .flush)))

(defn normal-mode? [vim]
  (= 'NORMAL (v/get-mode vim)))

(defn ready-to-append? [vim inputs]
  (and (seq inputs)
       (normal-mode? vim)
       (not= :repl-out (:current-tab @c/*state))))

(defn apply-parinfer! [{:keys [current-buffer] :as state} vim]
  (let [{:keys [parsed-code needs-parinfer?]} (get-in state [:buffers current-buffer])]
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
      (swap! c/*state
        (fn [state]
          (-> state
              (assoc-in [:buffers current-buffer :needs-parinfer?] false)
              (c/update-buffers)))))))

(defn read-text-resource [path]
  (-> path io/resource slurp str/split-lines))

(def current-year (.getYear (LocalDate/now)))
(def ascii-art {"smile" nil
                "intro" nil
                "cat" (LocalDate/of current-year 8 8)
                "usa" (LocalDate/of current-year 7 4)
                "christmas" (LocalDate/of current-year 12 25)})

(defn assoc-ascii [state ascii-name]
  (-> state
      (assoc-in [:buffers ascii-name] (c/->ascii state (read-text-resource (str "ascii/" ascii-name ".txt"))))
      (assoc :ascii ascii-name)))

(defn dissoc-ascii [state ascii-name]
  (-> state
      (update :buffers dissoc ascii-name)
      (assoc :ascii nil)))

(defn change-ascii [{:keys [mode command-text command-start ascii] :as state} s]
  (cond
    (and ascii *update-ui?*)
    (dissoc-ascii state ascii)
    (and (= mode 'COMMAND_LINE)
         (= s "<Enter>")
         (= command-start ":")
         (contains? ascii-art command-text))
    (assoc-ascii state command-text)
    :else
    state))

(defn init-ascii []
  (let [now (LocalDate/now)]
    (some->> (or (some (fn [[ascii date]]
                         (when (= now date)
                           ascii))
                       ascii-art)
                 "intro")
             (swap! c/*state assoc-ascii))))

(defn input [{:keys [mode command-text command-start] :as state} vim s]
  (if (and (= 'COMMAND_LINE mode) command-text)
    (let [pos (v/get-command-position vim)]
      (case s
        "<Tab>"
        (when (= (count command-text) pos)
          (when-let [completion-text (v/get-command-completion vim)]
            (when-let [first-part (str/last-index-of command-text " ")]
              (dotimes [_ (- (count command-text) (inc first-part))]
                (v/input vim "<BS>"))
              (doseq [ch completion-text]
                (v/input vim (str ch))))))
        ("<Right>" "<Left>" "<Up>" "<Down>")
        nil
        (v/input vim s)))
    (v/input vim s)))

(defn update-selection [buffer state vim]
  (if (v/visual-active? vim)
    (let [visual-range (-> (v/get-visual-range vim)
                           (update :start-line dec)
                           (update :end-line dec))]
      (c/update-selection buffer state visual-range))
    buffer))

(defn update-search-highlights [{:keys [visible-start-line visible-end-line] :as buffer} {:keys [show-search?] :as state} vim]
  (if (and show-search? visible-start-line visible-end-line)
    (let [highlights (mapv (fn [highlight]
                             (-> highlight
                                 (update :start-line dec)
                                 (update :end-line dec)))
                           (v/get-search-highlights vim (inc visible-start-line) (inc visible-end-line)))]
      (c/update-search-highlights buffer state highlights))
    buffer))

(defn update-buffer [buffer state game vim]
  (-> buffer
      (c/update-cursor state game)
      (c/update-highlight state)
      (update-selection state vim)
      (update-search-highlights state vim)))

(defn update-state-after-input [state game vim s]
  (let [current-buffer (v/get-current-buffer vim)
        old-mode (:mode state)
        mode (v/get-mode vim)
        cursor-line (dec (v/get-cursor-line vim))
        cursor-column (v/get-cursor-column vim)]
    (as-> state state
          (change-ascii state s)
          (assoc state :mode mode)
          (if (and (not= 'COMMAND_LINE old-mode)
                   (= mode 'COMMAND_LINE))
            (assoc state :command-start s)
            state)
          (if (and (= (:command-start state) "/")
                   (= mode 'COMMAND_LINE))
            (assoc state :show-search? true)
            state)
          (c/update-command state (v/get-command-text vim) (v/get-command-position vim))
          (if (c/get-buffer state current-buffer)
            (as-> state state
                  (update-in state [:buffers current-buffer] assoc :cursor-line cursor-line :cursor-column cursor-column)
                  (c/update-buffers state)
                  (update-in state [:buffers current-buffer] update-buffer state game vim))
            state))))

(defn on-input [game vim s]
  (input @c/*state vim s)
  (let [state (swap! c/*state update-state-after-input game vim s)]
    (when (and (= 'NORMAL (:mode state))
               (not= s "u"))
      (apply-parinfer! state vim))))

(defn append-to-buffer! [game vim inputs]
  (when-let [buffer (-> @c/*state :tab->buffer :repl-out)]
    (let [current-buffer (v/get-current-buffer vim)
          cursor-line (v/get-cursor-line vim)
          cursor-column (v/get-cursor-column vim)
          _ (v/set-current-buffer vim buffer)
          line-count (v/get-line-count vim buffer)
          char-count (count (v/get-line vim buffer line-count))]
      (v/set-cursor-position vim line-count (dec char-count))
      (on-input game vim "a")
      (doseq [input inputs
              ch input]
        (on-input game vim (str ch)))
      (on-input game vim "<Esc>")
      (v/set-current-buffer vim current-buffer)
      (v/set-cursor-position vim cursor-line cursor-column))))

(defn on-buf-enter [game vim buffer-ptr]
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
                              c/tab->path)
                            :files)
            state @c/*state
            buffer (or (c/get-buffer state buffer-ptr)
                       (assoc (c/->buffer state path file-name lines current-tab)
                         :cursor-line (dec (v/get-cursor-line vim))
                         :cursor-column (v/get-cursor-column vim)))
            buffer (if (:clojure? buffer)
                      (-> buffer
                          (c/parse-clojure-buffer state true)
                          (c/update-clojure-buffer state))
                      buffer)]
        (swap! c/*state
          (fn [state]
            (-> state
                (assoc :current-buffer buffer-ptr :current-tab current-tab)
                (update :tab->buffer assoc current-tab buffer-ptr)
                (assoc-in [:buffers buffer-ptr] (c/update-cursor buffer state game))))))
      ;; clear the files tab
      (swap! c/*state
        (fn [state]
          (-> state
              (assoc :current-buffer nil)
              (update :tab->buffer assoc :files nil)))))))

(defn on-buf-update [vim buffer-ptr start-line end-line line-count-change]
  (let [first-line (dec start-line)
        last-line (+ (dec end-line) line-count-change)
        lines (vec (for [i (range first-line last-line)]
                     (v/get-line vim buffer-ptr (inc i))))]
    (swap! c/*state update :buffer-updates conj {:buffer-ptr buffer-ptr
                                                 :lines lines
                                                 :first-line first-line
                                                 :line-count-change line-count-change})))

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
    (v/execute "filetype plugin indent on")))

(defn init [vim game]
  (v/set-on-quit vim (fn [buffer-ptr force?]
                       (System/exit 0)))
  (v/set-on-auto-command vim (fn [buffer-ptr event]
                               (case event
                                 EVENT_BUFENTER
                                 (when *update-ui?*
                                   (on-buf-enter game vim buffer-ptr))
                                 nil)))
  (v/set-on-buffer-update vim (partial on-buf-update vim))
  (v/set-on-stop-search-highlight vim (fn []
                                        (swap! c/*state assoc :show-search? false)))
  (v/set-on-unhandled-escape vim (fn []
                                   (swap! c/*state
                                     (fn [{:keys [current-buffer] :as state}]
                                       (let [state (assoc state :show-search? false)]
                                         (if (c/get-buffer state current-buffer)
                                           (update-in state [:buffers current-buffer] update-buffer state game vim)
                                           state))))))
  (run! #(v/open-buffer vim (c/tab->path %)) [:repl-in :repl-out :files])
  (init-ascii))

