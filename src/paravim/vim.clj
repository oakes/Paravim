(ns paravim.vim
  (:import [org.lwjgl.system Library CallbackI$V MemoryUtil]
           [org.lwjgl.system.dyncall DynCall DynCallback]))

(defprotocol IBuffer
  (get-line [this line-num])
  (get-line-count [this]))

(defn ->buffer [lib vm buffer-ptr]
  (let [get-line* (.getFunctionAddress lib "vimBufferGetLine")
        get-line-count* (.getFunctionAddress lib "vimBufferGetLineCount")]
    (reify IBuffer
      (get-line [this line-num]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm buffer-ptr)
        (DynCall/dcArgLong vm line-num)
        (-> (DynCall/dcCallPointer vm get-line*)
            MemoryUtil/memUTF8))
      (get-line-count [this]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm buffer-ptr)
        (DynCall/dcCallLong vm get-line-count*)))))

(defprotocol IVim
  (init [this])
  (open-buffer [this file-name])
  (set-buffer-update [this callback])
  (get-cursor-column [this])
  (get-cursor-line [this])
  (input [this input])
  (execute [this cmd])
  (set-quit [this callback])
  (set-tab-size [this size])
  (get-tab-size [this]))

(defn ->vim []
  (let [lib (Library/loadNative "libvim")
        init* (.getFunctionAddress lib "vimInit")
        open-buffer* (.getFunctionAddress lib "vimBufferOpen")
        set-buffer-update* (.getFunctionAddress lib "vimSetDestructuredBufferUpdateCallback")
        get-cursor-column* (.getFunctionAddress lib "vimCursorGetColumn")
        get-cursor-line* (.getFunctionAddress lib "vimCursorGetLine")
        input* (.getFunctionAddress lib "vimInput")
        execute* (.getFunctionAddress lib "vimExecute")
        set-quit* (.getFunctionAddress lib "vimSetQuitCallback")
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
        (->buffer lib vm (DynCall/dcCallPointer vm open-buffer*)))
      (set-buffer-update [this callback]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (MemoryUtil/memAddressSafe
                                   (reify CallbackI$V
                                     (callback [this args]
                                       (let [buf (DynCallback/dcbArgPointer args)
                                             start-line (DynCallback/dcbArgLong args)
                                             end-line (DynCallback/dcbArgLong args)
                                             line-count (DynCallback/dcbArgLong args)]
                                         (callback start-line end-line line-count)))
                                     (getSignature [this]
                                       "(plll)v"))))
        (DynCall/dcCallVoid vm set-buffer-update*))
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
      (set-quit [this callback]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (MemoryUtil/memAddressSafe
                                   (reify CallbackI$V
                                     (callback [this args]
                                       (callback (DynCallback/dcbArgPointer args)
                                                 (DynCallback/dcbArgBool args)))
                                     (getSignature [this]
                                       "(pb)v"))))
        (DynCall/dcCallVoid vm set-quit*))
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

