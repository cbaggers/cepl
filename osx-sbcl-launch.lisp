(sdl2:make-this-thread-main
 (lambda ()
   (cepl:repl)
   (swank:create-server :style nil)))
