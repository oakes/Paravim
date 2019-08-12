(ns paravim.vim
  (:import [org.lwjgl.system Library CallbackI$V MemoryUtil]
           [org.lwjgl.system.dyncall DynCall DynCallback]))

(def auto-events
  '[EVENT_BUFADD,               ;; after adding a buffer to the buffer list
    EVENT_BUFDELETE,            ;; deleting a buffer from the buffer list
    EVENT_BUFENTER,             ;; after entering a buffer
    EVENT_BUFFILEPOST,          ;; after renaming a buffer
    EVENT_BUFFILEPRE,           ;; before renaming a buffer
    EVENT_BUFHIDDEN,            ;; just after buffer becomes hidden
    EVENT_BUFLEAVE,             ;; before leaving a buffer
    EVENT_BUFNEW,               ;; after creating any buffer
    EVENT_BUFNEWFILE,           ;; when creating a buffer for a new file
    EVENT_BUFREADCMD,           ;; read buffer using command
    EVENT_BUFREADPOST,          ;; after reading a buffer
    EVENT_BUFREADPRE,           ;; before reading a buffer
    EVENT_BUFUNLOAD,            ;; just before unloading a buffer
    EVENT_BUFWINENTER,          ;; after showing a buffer in a window
    EVENT_BUFWINLEAVE,          ;; just after buffer removed from window
    EVENT_BUFWIPEOUT,           ;; just before really deleting a buffer
    EVENT_BUFWRITECMD,          ;; write buffer using command
    EVENT_BUFWRITEPOST,         ;; after writing a buffer
    EVENT_BUFWRITEPRE,          ;; before writing a buffer
    EVENT_CMDLINECHANGED,       ;; command line was modified
    EVENT_CMDLINEENTER,         ;; after entering the command line
    EVENT_CMDLINELEAVE,         ;; before leaving the command line
    EVENT_CMDUNDEFINED,         ;; command undefined
    EVENT_CMDWINENTER,          ;; after entering the cmdline window
    EVENT_CMDWINLEAVE,          ;; before leaving the cmdline window
    EVENT_COLORSCHEME,          ;; after loading a colorscheme
    EVENT_COLORSCHEMEPRE,       ;; before loading a colorscheme
    EVENT_COMPLETECHANGED,      ;; after completion popup menu changed
    EVENT_COMPLETEDONE,         ;; after finishing insert complete
    EVENT_CURSORHOLD,           ;; cursor in same position for a while
    EVENT_CURSORHOLDI,          ;; idem, in Insert mode
    EVENT_CURSORMOVED,          ;; cursor was moved
    EVENT_CURSORMOVEDI,         ;; cursor was moved in Insert mode
    EVENT_DIFFUPDATED,          ;; after diffs were updated
    EVENT_DIRCHANGED,           ;; after user changed directory
    EVENT_ENCODINGCHANGED,      ;; after changing the 'encoding' option
    EVENT_EXITPRE,              ;; before exiting
    EVENT_FILEAPPENDCMD,        ;; append to a file using command
    EVENT_FILEAPPENDPOST,       ;; after appending to a file
    EVENT_FILEAPPENDPRE,        ;; before appending to a file
    EVENT_FILECHANGEDRO,        ;; before first change to read-only file
    EVENT_FILECHANGEDSHELL,     ;; after shell command that changed file
    EVENT_FILECHANGEDSHELLPOST, ;; after (not) reloading changed file
    EVENT_FILEREADCMD,          ;; read from a file using command
    EVENT_FILEREADPOST,         ;; after reading a file
    EVENT_FILEREADPRE,          ;; before reading a file
    EVENT_FILETYPE,             ;; new file type detected (user defined)
    EVENT_FILEWRITECMD,         ;; write to a file using command
    EVENT_FILEWRITEPOST,        ;; after writing a file
    EVENT_FILEWRITEPRE,         ;; before writing a file
    EVENT_FILTERREADPOST,       ;; after reading from a filter
    EVENT_FILTERREADPRE,        ;; before reading from a filter
    EVENT_FILTERWRITEPOST,      ;; after writing to a filter
    EVENT_FILTERWRITEPRE,       ;; before writing to a filter
    EVENT_FOCUSGAINED,          ;; got the focus
    EVENT_FOCUSLOST,            ;; lost the focus to another app
    EVENT_FUNCUNDEFINED,        ;; if calling a function which doesn't exist
    EVENT_GUIENTER,             ;; after starting the GUI
    EVENT_GUIFAILED,            ;; after starting the GUI failed
    EVENT_INSERTCHANGE,         ;; when changing Insert/Replace mode
    EVENT_INSERTCHARPRE,        ;; before inserting a char
    EVENT_INSERTENTER,          ;; when entering Insert mode
    EVENT_INSERTLEAVE,          ;; when leaving Insert mode
    EVENT_MENUPOPUP,            ;; just before popup menu is displayed
    EVENT_OPTIONSET,            ;; option was set
    EVENT_QUICKFIXCMDPOST,      ;; after :make, :grep etc.
    EVENT_QUICKFIXCMDPRE,       ;; before :make, :grep etc.
    EVENT_QUITPRE,              ;; before :quit
    EVENT_REMOTEREPLY,          ;; upon string reception from a remote vim
    EVENT_SESSIONLOADPOST,      ;; after loading a session file
    EVENT_SHELLCMDPOST,         ;; after ":!cmd"
    EVENT_SHELLFILTERPOST,      ;; after ":1,2!cmd", ":w !cmd", ":r !cmd".
    EVENT_SOURCECMD,            ;; sourcing a Vim script using command
    EVENT_SOURCEPRE,            ;; before sourcing a Vim script
    EVENT_SOURCEPOST,           ;; after sourcing a Vim script
    EVENT_SPELLFILEMISSING,     ;; spell file missing
    EVENT_STDINREADPOST,        ;; after reading from stdin
    EVENT_STDINREADPRE,         ;; before reading from stdin
    EVENT_SWAPEXISTS,           ;; found existing swap file
    EVENT_SYNTAX,               ;; syntax selected
    EVENT_TABCLOSED,            ;; after closing a tab page
    EVENT_TABENTER,             ;; after entering a tab page
    EVENT_TABLEAVE,             ;; before leaving a tab page
    EVENT_TABNEW,               ;; when entering a new tab page
    EVENT_TERMCHANGED,          ;; after changing 'term'
    EVENT_TERMINALOPEN,         ;; after a terminal buffer was created
    EVENT_TERMRESPONSE,         ;; after setting "v:termresponse"
    EVENT_TEXTCHANGED,          ;; text was modified not in Insert mode
    EVENT_TEXTCHANGEDI,         ;; text was modified in Insert mode
    EVENT_TEXTCHANGEDP,         ;; TextChangedI with popup menu visible
    EVENT_TEXTYANKPOST,         ;; after some text was yanked
    EVENT_USER,                 ;; user defined autocommand
    EVENT_VIMENTER,             ;; after starting Vim
    EVENT_VIMLEAVE,             ;; before exiting Vim
    EVENT_VIMLEAVEPRE,          ;; before exiting Vim and writing .viminfo
    EVENT_VIMRESIZED,           ;; after Vim window was resized
    EVENT_WINENTER,             ;; after entering a window
    EVENT_WINLEAVE,             ;; before leaving a window
    EVENT_WINNEW,               ;; when entering a new window
    NUM_EVENTS                  ;; MUST be the last one
  ])

(defprotocol IVim
  (init [this])
  (open-buffer [this file-name])
  (get-current-buffer [this])
  (get-line [this buffer-ptr line-num])
  (get-line-count [this buffer-ptr])
  (set-on-buffer-update [this callback])
  (set-on-auto-command [this callback])
  (get-command-text [this])
  (get-command-position [this])
  (get-command-completion [this])
  (get-cursor-column [this])
  (get-cursor-line [this])
  (input [this input])
  (execute [this cmd])
  (set-on-quit [this callback])
  (set-tab-size [this size])
  (get-tab-size [this]))

(defn ->vim []
  (let [lib (Library/loadNative "libvim")
        init* (.getFunctionAddress lib "vimInit")
        open-buffer* (.getFunctionAddress lib "vimBufferOpen")
        get-current-buffer* (.getFunctionAddress lib "vimBufferGetCurrent")
        get-line* (.getFunctionAddress lib "vimBufferGetLine")
        get-line-count* (.getFunctionAddress lib "vimBufferGetLineCount")
        set-on-buffer-update* (.getFunctionAddress lib "vimSetDestructuredBufferUpdateCallback")
        set-on-auto-command* (.getFunctionAddress lib "vimSetAutoCommandCallback")
        get-command-text* (.getFunctionAddress lib "vimCommandLineGetText")
        get-command-position* (.getFunctionAddress lib "vimCommandLineGetPosition")
        get-command-completion* (.getFunctionAddress lib "vimCommandLineGetCompletion")
        get-cursor-column* (.getFunctionAddress lib "vimCursorGetColumn")
        get-cursor-line* (.getFunctionAddress lib "vimCursorGetLine")
        input* (.getFunctionAddress lib "vimInput")
        execute* (.getFunctionAddress lib "vimExecute")
        set-on-quit* (.getFunctionAddress lib "vimSetQuitCallback")
        set-tab-size* (.getFunctionAddress lib "vimOptionSetTabSize")
        get-tab-size* (.getFunctionAddress lib "vimOptionGetTabSize")
        vm (DynCall/dcNewCallVM 1024)]
    (reify IVim
      (init [this]
        (DynCall/dcMode vm DynCall/DC_CALL_C_DEFAULT)
        (DynCall/dcReset vm)
        (DynCall/dcCallVoid vm init*))
      (open-buffer [this file-name]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (-> file-name MemoryUtil/memUTF8 MemoryUtil/memAddress))
        (DynCall/dcArgLong vm 1)
        (DynCall/dcArgInt vm 0)
        (DynCall/dcCallPointer vm open-buffer*))
      (get-current-buffer [this]
        (DynCall/dcReset vm)
        (DynCall/dcCallPointer vm get-current-buffer*))
      (get-line [this buffer-ptr line-num]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm buffer-ptr)
        (DynCall/dcArgLong vm line-num)
        (-> (DynCall/dcCallPointer vm get-line*)
            MemoryUtil/memUTF8))
      (get-line-count [this buffer-ptr]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm buffer-ptr)
        (DynCall/dcCallLong vm get-line-count*))
      (set-on-buffer-update [this callback]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (MemoryUtil/memAddressSafe
                                   (reify CallbackI$V
                                     (callback [this args]
                                       (let [buffer-ptr (DynCallback/dcbArgPointer args)
                                             start-line (DynCallback/dcbArgLong args)
                                             end-line (DynCallback/dcbArgLong args)
                                             line-count (DynCallback/dcbArgLong args)]
                                         (callback buffer-ptr start-line end-line line-count)))
                                     (getSignature [this]
                                       "(plll)v"))))
        (DynCall/dcCallVoid vm set-on-buffer-update*))
      (set-on-auto-command [this callback]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (MemoryUtil/memAddressSafe
                                   (reify CallbackI$V
                                     (callback [this args]
                                       (let [event (DynCallback/dcbArgInt args)
                                             buffer-ptr (DynCallback/dcbArgPointer args)]
                                         (callback buffer-ptr (auto-events event))))
                                     (getSignature [this]
                                       "(ip)v"))))
        (DynCall/dcCallVoid vm set-on-auto-command*))
      (get-command-text [this]
        (DynCall/dcReset vm)
        (let [ptr (DynCall/dcCallPointer vm get-command-text*)]
          (when (> ptr 0)
            (MemoryUtil/memUTF8 ptr))))
      (get-command-position [this]
        (DynCall/dcReset vm)
        (DynCall/dcCallInt vm get-command-position*))
      (get-command-completion [this]
        (DynCall/dcReset vm)
        (let [ptr (DynCall/dcCallPointer vm get-command-completion*)]
          (when (> ptr 0)
            (MemoryUtil/memUTF8 ptr))))
      (get-cursor-column [this]
        (DynCall/dcReset vm)
        (DynCall/dcCallInt vm get-cursor-column*))
      (get-cursor-line [this]
        (DynCall/dcReset vm)
        (DynCall/dcCallLong vm get-cursor-line*))
      (input [this input]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (-> input MemoryUtil/memUTF8 MemoryUtil/memAddress))
        (DynCall/dcCallVoid vm input*))
      (execute [this cmd]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (-> cmd MemoryUtil/memUTF8 MemoryUtil/memAddress))
        (DynCall/dcCallVoid vm execute*))
      (set-on-quit [this callback]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (MemoryUtil/memAddressSafe
                                   (reify CallbackI$V
                                     (callback [this args]
                                       (let [buffer-ptr (DynCallback/dcbArgPointer args)
                                             force? (DynCallback/dcbArgBool args)]
                                         (callback buffer-ptr force?)))
                                     (getSignature [this]
                                       "(pb)v"))))
        (DynCall/dcCallVoid vm set-on-quit*))
      (set-tab-size [this size]
        (DynCall/dcReset vm)
        (DynCall/dcArgInt vm size)
        (DynCall/dcCallVoid vm set-tab-size*))
      (get-tab-size [this]
        (DynCall/dcReset vm)
        (DynCall/dcCallInt vm get-tab-size*)))))

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

