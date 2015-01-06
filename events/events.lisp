(in-package :cepl.events)

(defparameter *inputs* (make-hash-table))

;; really basic event system

(defclass event-node ()
  ((body :initarg :body :initform #'identity)
   (subscribing-nodes :initarg :nodes :initform nil)
   (subscribing-funcs :initarg :funcs :initform nil)))

(defclass event-expand-node (event-node) ())
(defclass event-pump-func-node (event-node) ())
(defclass event-filter-node (event-node) ())

(defmethod subscribe ((subscriber event-node) (subscribe-to event-node))
  (push subscriber (slot-value subscribe-to 'subscribing-nodes))
  (lambda () (delete subscriber (slot-value subscribe-to 'subscribing-nodes))))

(defmethod subscribe ((subscriber function) (subscribe-to event-node))
  (push subscriber (slot-value subscribe-to 'subscribing-funcs))
  (lambda () (delete subscriber (slot-value subscribe-to 'subscribing-funcs))))

(defmethod event-push (event (node event-node))  
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (let ((result (funcall body event)))
      (loop :for node :in nodes :do (event-push result node))
      (loop :for func :in funcs :do (funcall func result)))))

(defmethod event-push (event (node event-node))  
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (when (funcall body event)
      (loop :for node :in nodes :do (event-push event node))
      (loop :for func :in funcs :do (funcall func event)))))

(defmethod event-push (event (node event-expand-node))
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (let ((results (funcall body event)))
      (loop :for result :in results :do
         (loop :for node :in nodes :do (event-push result node))
         (loop :for func :in funcs :do (funcall func result))))))

(defmethod event-push (event (node event-pump-func-node))
  (declare (ignore event))
  (with-slots (body (nodes subscribing-nodes) (funcs subscribing-funcs)) node
    (loop :for result = (funcall body) :while result :do 
       (loop :for node :in nodes :do (event-push result node))
       (loop :for func :in funcs :do (funcall func result)))))


(defun pump-sdl-events ()
  (event-push nil (gethash 'sdl-all-events *inputs*)))
(defun create-sdl-event-sources ()
  (let ((all-events (make-instance
                     'event-expand-node :body
                     (lambda (_) (declare (ignore _)) (collect-sdl-events)))))
    (setf (gethash 'sdl-all-events *inputs*) all-events)
    (subscribe 
     (setf (gethash :sdl-sys *inputs*)
           (make-instance 'event-filter-node :body #'sdl-quit-event-p))
     all-events)
    (subscribe 
     (setf (gethash :mouse *inputs*)
           (make-instance
            'event-filter-node :body
            (lambda (event)
              event
              (or (and (sdl-mouse-scroll-event-p event)
                       (= (sdl-mouse-scroll-event-source-id event) 0))
                  (and (sdl-mouse-button-event-p event)
                       (= (sdl-mouse-button-event-source-id event) 0))
                  (and (sdl-mouse-motion-event-p event)
                       (= (sdl-mouse-motion-event-source-id event) 0))))))
     all-events)
    (subscribe
     (setf (gethash :keyboard *inputs*)
           (make-instance 'event-filter-node :body #'sdl-key-event-p))
     all-events)))
(create-sdl-event-sources)




;;--------------------------------------------
;; sdl event helpers

;; {TODO} optimize
(let ((sdl->lisp-time-offset 0))
  (defun set-sdl->lisp-time-offset ()
    (setf sdl->lisp-time-offset (- (get-internal-real-time) (sdl2::get-ticks))))
  (defun sdl->lisp-time (sdl-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (+ sdl-time sdl->lisp-time-offset))
  (defun lisp->sdl-time (lisp-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (- lisp-time sdl->lisp-time-offset)))

(defstruct sdl-event
  (timestamp 0 :type fixnum))
(defstruct sdl-quit-event
  (timestamp 0 :type fixnum))
(defstruct sdl-window-event
  (timestamp 0 :type fixnum)
  (action nil :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum))
(defstruct sdl-mouse-scroll-event
  (timestamp 0 :type fixnum)
  (source-id 0 :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum))
(defstruct sdl-mouse-button-event
  (timestamp 0 :type fixnum)
  (source-id 0 :type fixnum)
  (button 0 :type fixnum)
  (state 0 :type fixnum)
  (clicks 0 :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum))
(defstruct sdl-mouse-motion-event
  (timestamp 0 :type fixnum)
  (source-id 0 :type fixnum)
  (state 0 :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (x-rel 0 :type fixnum)
  (y-rel 0 :type fixnum))
(defstruct sdl-key-event
  (timestamp 0 :type fixnum)
  (state 0 :type fixnum)
  (repeat 0 :type boolean)
  (key 0 :type keyword))


(defun collect-sdl-events ()  
  (let ((event (sdl2:new-event)))
    (prog1
        (loop :until (= 0 (sdl2:next-event event :poll nil)) :collect
           (make-sdl-event))
      (sdl2:free-event event))))

;; (AUTOWRAP:DEFINE-FOREIGN-RECORD 'SDL-KEYSYM :STRUCT 128 32
;;                                     '((:SCANCODE SDL-SCANCODE :BIT-SIZE 32
;;                                        :BIT-OFFSET 0 :BIT-ALIGNMENT 32)
;;                                       (:SYM SDL-KEYCODE :BIT-SIZE 32 :BIT-OFFSET 32
;;                                        :BIT-ALIGNMENT 32)
;;                                       (:MOD UINT16 :BIT-SIZE 16 :BIT-OFFSET 64
;;                                        :BIT-ALIGNMENT 16)
;;                                       (:UNUSED UINT32 :BIT-SIZE 32 :BIT-OFFSET 96
;;                                        :BIT-ALIGNMENT 32)))

(defun collect-sdl-events ()
  (let ((results nil))
    (case-events (event)
      (:quit (:timestamp ts)
             (push (make-sdl-quit-event :timestamp ts) results))

      (:windowevent (:timestamp ts :event e :data1 x :data2 y)
                    (push (make-sdl-window-event
                           :timestamp (sdl->lisp-time ts) :action e :x x :y y)
                          results))

      (:mousewheel (:timestamp ts :which id :x x :y y)
                   (push (make-sdl-mouse-scroll-event
                          :timestamp (sdl->lisp-time ts) :source-id id :x x :y y)
                         results))

      ((:mousebuttondown :mousebuttonup) 
       (:timestamp ts :which id :button b :state s :clicks c :x x :y y)
       (push (make-sdl-mouse-button-event
              :timestamp (sdl->lisp-time ts) :source-id id :button b :state s
              :clicks c :x x :y y)
             results))

      (:mousemotion
       (:timestamp ts :which id :state s :x x :y y :xrel xrel :yrel yrel)
       (push (make-sdl-mouse-motion-event
              :timestamp (sdl->lisp-time ts) :source-id id :state s 
              :x x :y y :x-rel xrel :y-rel yrel)
             results))
      
      ((:keydown :keyup)
       (:timestamp ts :state s :repeat r :keysym keysym)       
       (push (make-sdl-key-event 
              :timestamp (sdl->lisp-time ts) :state s :repeat (= r 0)
              :key (sdl-scancode-lookup 
                    (plus-c:c-ref keysym sdl2-ffi:sdl-keysym :scancode)))
             results)))
    results))

(defun sdl-scancode-lookup (scancode)
  (aref *sdl-scan-lookup* scancode))

;; {TODO}
;; :textediting
;; :textinput

;; :joyaxismotion
;; :joyballmotion
;; :joyhatmotion
;; :joybuttondown
;; :joybuttonup
;; :joydeviceadded
;; :joydeviceremoved

;; :controlleraxismotion
;; :controllerbuttondown
;; :controllerbuttonup
;; :controllerdeviceadded
;; :controllerdeviceremoved
;; :controllerdeviceremapped

;; :fingerdown
;; :fingerup
;; :fingermotion

;; :multigesture

;; :clipboardupdate

;; :dropfile

;; :render-targets-reset

;; :userevent
;; :lastevent

(defmacro case-events ((event &key (method :poll) (timeout nil))
                       &body event-handlers)
  `(let (,(when (symbolp event) `(,event (sdl2:new-event))))
     (loop :until (= 0  (sdl2:next-event ,event ,method ,timeout)) :do
        (case (sdl2::get-event-type ,event)
          ,@(loop :for (type params . forms) :in event-handlers :collect
               (expand-handler event type params forms) :into results
               :finally (return (remove nil results)))))
     (sdl2:free-event ,event)))

(defun expand-handler (event type params forms)
  (if (listp type)
      (sdl2::expand-handler event (first type) params forms)
      (sdl2::expand-handler event type params forms)))

(defun collect-event-types ()
  (let* ((x (sdl2:new-event))
         (event-types (loop :until (= 0 (sdl2::sdl-poll-event x))
                         :collect (sdl2::get-event-type x))))
    (sdl2:free-event x)
    event-types))

(defun map-sdl-events (function &rest more-functions)
  (let ((event (sdl2:new-event)))
    (loop :until (= 0 (sdl2:next-event event :poll nil)) :do
       (funcall function event)
       (dolist (f more-functions) (funcall f event)))
    (sdl2:free-event event)))

(defparameter *sdl-scan-lookup* 
  #(:unknown nil nil nil :a :b
    :c :d :e :f
    :g :h :i :j
    :k :l :m :n
    :o :p :q :r
    :s :t :u :v
    :w :x :y :z
    :1 :2 :3 :4
    :5 :6 :7 :8
    :9 :0 :return :escape
    :backspace :tab :space
    :minus :equals :leftbracket
    :rightbracket :backslash :nonushash
    :semicolon :apostrophe :grave
    :comma :period :slash
    :capslock :f1 :f2 :f3
    :f4 :f5 :f6 :f7
    :f8 :f9 :f10 :f11
    :f12 :printscreen :scrolllock
    :pause :insert :home
    :pageup :delete :end
    :pagedown :right :left
    :down :up :numlockclear
    :kp_divide :kp_multiply :kp_minus
    :kp_plus :kp_enter :kp_1
    :kp_2 :kp_3 :kp_4 :kp_5
    :kp_6 :kp_7 :kp_8 :kp_9
    :kp_0 :kp_period :nonusbackslash
    :application :power :kp_equals
    :f13 :f14 :f15 :f16
    :f17 :f18 :f19 :f20
    :f21 :f22 :f23 :f24
    :execute :help :menu
    :select :stop :again
    :undo :cut :copy :paste
    :find :mute :volumeup
    :volumedown :lockingcapslock
    :lockingnumlock :lockingscrolllock
    :kp_comma :kp_equalsas400
    :international1 :international2
    :international3 :international4
    :international5 :international6
    :international7 :international8
    :international9 :lang1 :lang2
    :lang3 :lang4 :lang5
    :lang6 :lang7 :lang8
    :lang9 :alterase :sysreq
    :cancel :clear :prior
    :return2 :separator :out
    :oper :clearagain :crsel
    :exsel nil nil nil nil nil nil nil nil nil nil nil
    :kp_00 :kp_000 :thousandsseparator
    :decimalseparator :currencyunit
    :currencysubunit :kp_leftparen
    :kp_rightparen :kp_leftbrace
    :kp_rightbrace :kp_tab :kp_backspace
    :kp_a :kp_b :kp_c :kp_d
    :kp_e :kp_f :kp_xor
    :kp_power :kp_percent :kp_less
    :kp_greater :kp_ampersand
    :kp_dblampersand :kp_verticalbar
    :kp_dblverticalbar :kp_colon :kp_hash
    :kp_space :kp_at :kp_exclam
    :kp_memstore :kp_memrecall
    :kp_memclear :kp_memadd
    :kp_memsubtract :kp_memmultiply
    :kp_memdivide :kp_plusminus :kp_clear
    :kp_clearentry :kp_binary :kp_octal
    :kp_decimal :kp_hexadecimal nil nil
    :lctrl :lshift :lalt
    :lgui :rctrl :rshift
    :ralt :rgui nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :mode :audionext :audioprev
    :audiostop :audioplay :audiomute
    :mediaselect :www :mail
    :calculator :computer :ac_search
    :ac_home :ac_back :ac_forward
    :ac_stop :ac_refresh :ac_bookmarks
    :brightnessdown :brightnessup
    :displayswitch :kbdillumtoggle
    :kbdillumdown :kbdillumup :eject
    :sleep))
