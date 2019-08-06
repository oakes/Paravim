(ns paravim.vim
  (:import [org.lwjgl.system MemoryUtil]
           [org.lwjgl.system Library CallbackI$V]
           [org.lwjgl.system.dyncall DynCall DynCallback]))

(defprotocol IVim
  (init [this])
  (set-tab-size [this size])
  (get-tab-size [this])
  (execute [this cmd])
  (set-quit [this callback]))

(defn ->vim []
  (let [lib (Library/loadNative "libvim")
        init (.getFunctionAddress lib "vimInit")
        set-tab-size (.getFunctionAddress lib "vimOptionSetTabSize")
        get-tab-size (.getFunctionAddress lib "vimOptionGetTabSize")
        set-quit (.getFunctionAddress lib "vimSetQuitCallback")
        exec (.getFunctionAddress lib "vimExecute")
        vm (DynCall/dcNewCallVM 1024)]
    (reify IVim
      (init [this]
        (DynCall/dcMode vm DynCall/DC_CALL_C_DEFAULT)
        (DynCall/dcReset vm)
        (DynCall/dcCallVoid vm init))
      (set-tab-size [this size]
        (DynCall/dcReset vm)
        (DynCall/dcArgInt vm size)
        (DynCall/dcCallVoid vm set-tab-size))
      (get-tab-size [this]
        (DynCall/dcReset vm)
        (DynCall/dcCallInt vm get-tab-size))
      (execute [this cmd]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (-> cmd MemoryUtil/memUTF8 MemoryUtil/memAddress))
        (DynCall/dcCallVoid vm exec))
      (set-quit [this callback]
        (DynCall/dcReset vm)
        (DynCall/dcArgPointer vm (MemoryUtil/memAddressSafe
                                   (reify CallbackI$V
                                     (callback [this args]
                                       (callback (DynCallback/dcbArgPointer args)
                                                 (DynCallback/dcbArgBool args)))
                                     (getSignature [this]
                                       "(pb)v"))))
        (DynCall/dcCallVoid vm set-quit)))))

