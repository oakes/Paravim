(ns paravim.vim
  (:require [paravim.core :as c]
            [libvim-clj.core :as v]
            [clojure.string :as str]
            [parinferish.core :as par]))

;; https://vim.fandom.com/wiki/Mapping_keys_in_Vim_-_Tutorial_%28Part_2%29

(def keyword->name
  {:backspace "<BS>"
   :tab "<Tab>"
   :enter "<Enter>"
   :escape "<Esc>"
   :up "<Up>"
   :down "<Down>"
   :left "<Left>"
   :right "<Right>"})

(defn open-buffer-for-tab! [vim {:keys [current-buffer current-tab tab->buffer] :as state}]
  (if-let [buffer-for-tab (tab->buffer current-tab)]
    (when (not= current-buffer buffer-for-tab)
      (v/set-current-buffer vim buffer-for-tab))
    (when-let [path (c/tab->path current-tab)]
      (v/open-buffer vim path))))

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

(defn split-inputs [vim inputs]
  (when (and (seq inputs) (normal-mode? vim))
    (let [current-buffer (v/get-current-buffer vim)
          *inputs-to-run (volatile! [])
          *inputs-to-delay (volatile! [])]
      (doseq [input inputs]
        (if (= (:buffer input) current-buffer)
          (vswap! *inputs-to-delay conj input)
          (vswap! *inputs-to-run conj input)))
      (let [inputs-to-run @*inputs-to-run]
        (when (seq inputs-to-run)
          [inputs-to-run @*inputs-to-delay current-buffer])))))

(defn append-to-buffer! [on-input vim current-buffer {:keys [buffer string]}]
  (let [cursor-line (v/get-cursor-line vim)
        cursor-column (v/get-cursor-column vim)
        line-count (v/get-line-count vim buffer)
        char-count (count (v/get-line vim buffer line-count))]
    (v/set-current-buffer vim buffer)
    (v/set-cursor-position vim line-count (dec char-count))
    (v/input vim "a")
    (doseq [ch string]
      (on-input (str ch)))
    (v/input vim "<Esc>")
    (v/set-current-buffer vim current-buffer)
    (v/set-cursor-position vim cursor-line cursor-column)))

(defn apply-parinfer! [{:keys [mode current-buffer] :as state} vim]
  (let [{:keys [parsed-code needs-parinfer?]} (get-in state [:buffers current-buffer])]
    (when (and needs-parinfer?
               ('#{NORMAL INSERT} mode))
      (let [cursor-line (v/get-cursor-line vim)
            cursor-column (v/get-cursor-column vim)
            diffs (par/diff parsed-code)]
        (when (seq diffs)
          (when (= 'NORMAL mode)
            (v/input vim "i"))
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
          (when (= 'NORMAL mode)
            (v/input vim "<Esc>"))))
      (swap! c/*state
        (fn [state]
          (-> state
              (assoc-in [:buffers current-buffer :needs-parinfer?] false)
              (c/update-buffers)))))))

(defn input [{:keys [mode command-text]} vim s]
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

(defn update-state-after-input [{:keys [mode] :as state} game vim s]
  (let [current-buffer (v/get-current-buffer vim)
        old-mode mode
        mode (v/get-mode vim)
        cursor-line (dec (v/get-cursor-line vim))
        cursor-column (v/get-cursor-column vim)]
    (-> state
        (assoc :mode mode)
        (cond-> (and (not= old-mode 'COMMAND_LINE)
                     (= mode 'COMMAND_LINE))
                (assoc :command-start s))
        (c/update-command (v/get-command-text vim) (v/get-command-position vim))
        (as-> state
              (if (c/get-buffer state current-buffer)
                (-> state
                    (update-in [:buffers current-buffer] assoc :cursor-line cursor-line :cursor-column cursor-column)
                    c/update-buffers
                    (c/update-cursor game current-buffer)
                    (c/update-highlight current-buffer)
                    (cond-> (v/visual-active? vim)
                            (c/update-selection current-buffer (-> (v/get-visual-range vim)
                                                                   (update :start-line dec)
                                                                   (update :end-line dec)))))
                state)))))

(defn on-buf-enter [game vim buffer-ptr]
  (let [cursor-line (dec (v/get-cursor-line vim))
        cursor-column (v/get-cursor-column vim)
        path (v/get-file-name vim buffer-ptr)
        lines (vec (for [i (range (v/get-line-count vim buffer-ptr))]
                (v/get-line vim buffer-ptr (inc i))))]
    (swap! c/*state
      (fn [{:keys [tab->buffer] :as state}]
        (as-> state state
              (if path
                (let [canon-path (-> path java.io.File. .getCanonicalPath)
                      current-tab (or (some
                                        (fn [[tab path]]
                                          (when (= canon-path (-> path java.io.File. .getCanonicalPath))
                                            tab))
                                        c/tab->path)
                                      :files)]
                  (-> state
                      (assoc :current-buffer buffer-ptr :current-tab current-tab)
                      (update :tab->buffer assoc current-tab buffer-ptr)))
                (-> state
                    (assoc :current-buffer nil)
                    (update :tab->buffer assoc :files nil)))
              (if (and path (nil? (c/get-buffer state buffer-ptr)))
                (as-> state state
                      (c/assoc-buffer state buffer-ptr path lines)
                      (if (:clojure? (c/get-buffer state buffer-ptr))
                        (-> state
                            (c/parse-clojure-buffer buffer-ptr true)
                            (c/update-clojure-buffer buffer-ptr))
                        state)
                      (update-in state [:buffers buffer-ptr] assoc :cursor-line cursor-line :cursor-column cursor-column))
                state)
              (if path
                (c/update-cursor state game buffer-ptr)
                state))))))

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
    (v/execute "filetype plugin indent on")))

(defn init [vim on-auto-command]
  (v/set-on-quit vim (fn [buffer-ptr force?]
                       (System/exit 0)))
  (v/set-on-auto-command vim on-auto-command)
  (v/set-on-buffer-update vim (partial on-buf-update vim))
  (run! #(v/open-buffer vim (c/tab->path %)) [:repl-in :repl-out :files]))
