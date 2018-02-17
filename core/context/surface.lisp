(in-package :cepl.context)

;;----------------------------------------------------------------------

(defun+ add-surface (context &key (title "CEPL") (width 600) (height 600)
                              (fullscreen nil) (resizable t) (no-frame nil)
                              (hidden nil) (make-current nil))
  (legacy-add-surface context title width height fullscreen resizable no-frame
                      hidden make-current nil))

(defun+ legacy-add-surface (context title width height fullscreen resizable
                                    no-frame hidden make-current gl-version)
  (when (> (length (%cepl-context-surfaces context)) 0)
    (assert (cepl.host:supports-multiple-surfaces-p) ()
            "CEPL: Sorry your current CEPL host does not currently support multiple surfaces "))
  (let* ((surface (make-instance 'pending-surface
                                 :title title
                                 :width width
                                 :height height
                                 :fullscreen fullscreen
                                 :resizable resizable
                                 :no-frame no-frame
                                 :hidden hidden
                                 :legacy-gl-version gl-version))
         (surface (if cepl.host::*current-host*
                      (make-surface-from-pending surface)
                      surface)))
    (%with-cepl-context-slots (surfaces) context
      (setf surfaces
            (if surfaces
                (append surfaces (list surface))
                (list surface))))
    (when make-current
      (make-surface-current context surface))
    context))

;;----------------------------------------------------------------------

(defun remove-surface (context surface)
  (%with-cepl-context-slots (surfaces) context
    (assert (find surface surfaces))
    (setf surfaces (remove surface surfaces))
    (cepl.host:destroy-surface surface)))

;;----------------------------------------------------------------------

(defun+ make-surface-current (cepl-context surface)
  (assert cepl-context)
  (assert surface)
  (%with-cepl-context-slots (gl-context surfaces current-surface) cepl-context
    (unless (eq surface current-surface)
      ;; GL may not be initialized yet
      (unless gl-context
        (init-gl-context cepl-context surface))
      ;;
      (assert (member surface surfaces))
      (cepl.host:make-gl-context-current-on-surface
       (handle gl-context) surface)
      (%set-default-fbo-and-viewport surface cepl-context)
      (setf current-surface surface))))

;;----------------------------------------------------------------------

(defun+ init-pending-surfaces (context)
  (%with-cepl-context-slots (surfaces) context
    (setf surfaces
          (mapcar (lambda (x)
                    (typecase x
                      (pending-surface (make-surface-from-pending x))
                      (t x)))
                  surfaces))))

(defun+ make-surface-from-pending (pending-surface)
  (assert cepl.host::*current-host* ()
          "CEPL: Cannot fully initialize surface without CEPL having been initialized")
  ;;
  (with-slots (title
               width height fullscreen resizable
               no-frame hidden legacy-gl-version)
      pending-surface
    (cepl.host::make-surface
     :title title :width width :height height
     :fullscreen fullscreen :resizable resizable
     :no-frame no-frame :hidden hidden
     :gl-version legacy-gl-version)))

;;----------------------------------------------------------------------

(defun+ surface-dimensions (surface)
  (cepl.host:window-size surface))

(defun+ surface-resolution (surface)
  (let ((data (cepl.host:window-size surface)))
    (make-array (length data) :element-type 'single-float
                :initial-contents data)))

(defun+ (setf surface-dimensions) (value surface)
  (destructuring-bind (width height) value
    (cepl.host:set-surface-size surface width height)))

(defun+ (setf surface-resolution) (value surface)
  (cepl.host:set-surface-size surface
                              (ceiling (aref value 0))
                              (ceiling (aref value 1))))

(defun+ surface-title (surface)
  (cepl.host:surface-title surface))

(defun+ (setf surface-title) (value surface)
  (cepl.host:set-surface-title surface value))

(defun+ surface-fullscreen-p (surface)
  (cepl.host:surface-fullscreen-p surface))

(defun+ (setf surface-fullscreen-p) (value surface)
  (cepl.host:set-surface-fullscreen surface value))
