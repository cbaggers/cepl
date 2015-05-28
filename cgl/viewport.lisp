(in-package :cgl)

(defvar *current-viewport* nil)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

;;{NOTE} if optimization called for it this could easily be an
;;       array of 16bit ints (or whatever works)
(defstruct (viewport (:conc-name %viewport-) (:constructor %make-viewport))
  (resolution-x 320 :type fixnum)
  (resolution-y 240 :type fixnum)
  (origin-x 0 :type fixnum)
  (origin-y 0 :type fixnum))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun %make-default-viewport (&optional (resolution '(320 240)) (origin '(0 0)))
  (let ((viewport (make-viewport resolution origin)))
    (setf *current-viewport* viewport)
    (%viewport viewport)
    viewport))

(defun make-viewport (&optional (resolution '(320 240)) (origin '(0 0)))
  (if (listp origin)      
      (%make-viewport :resolution-x (first resolution)
                      :resolution-y (second resolution)
                      :origin-x (first origin)
                      :origin-y (second origin))
      (%make-viewport :resolution-x (first resolution)
                      :resolution-y (second resolution)
                      :origin-x (v:x origin)
                      :origin-y (v:y origin))))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun viewport-resolution (viewport)
  (list (%viewport-resolution-x viewport)
        (%viewport-resolution-y viewport)))

(defun viewport-origin (viewport)
  (v! (%viewport-origin-x viewport)
      (%viewport-origin-y viewport)))

(defmethod cepl-generics:size ((object viewport))
  (v! (%viewport-origin-x object)
      (%viewport-origin-y object)))

(defmethod cepl-generics:pos ((object viewport))
  (v! (%viewport-origin-x object)
      (%viewport-origin-y object)))

(defun %set-resolution (viewport x y)
  (setf (%viewport-resolution-x viewport) x
        (%viewport-resolution-y viewport) y)
  (when (eq *current-viewport* viewport)
    (%viewport viewport)))

(defun (setf viewport-resolution) (value viewport)  
  (%set-resolution viewport (first value) (second value)))

(defmethod (setf cepl-generics:size) (value (object viewport))
  (%set-resolution object (v:x value) (v:y value)))
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(defun %viewport (viewport)
  (gl:viewport
   (%viewport-origin-x viewport) (%viewport-origin-y viewport)
   (%viewport-resolution-x viewport) (%viewport-resolution-y viewport)))

(defmacro %with-viewport (viewport &body body)
  `(progn
     (%viewport ,viewport)
     ,@body))

(defmacro with-viewport (viewport &body body)
  (let ((once (gensym "viewport")))
    `(prog1
         (let* ((,once ,viewport)
                (*current-viewport* ,once))
           (%with-viewport ,once ,@body))
       (%viewport *current-viewport*))))

;;{TODO} how are we meant to set origin?
;;       Well attachments dont have position so it wouldnt make sense
;;       you can however create other viewports and with with-viewport
;;       to make them current, then rendering with render to that viewport

(defmacro with-fbo-viewport ((fbo &optional (attachment 0)) &body body)
  `(with-viewport (attachment-viewport ,fbo ,attachment)
     ,@body))

(defmacro %with-fbo-viewport ((fbo &optional (attachment 0)) &body body)
  "To be used by code than is managing the viewport state itself. 
   composed dispatch would be an example"
  `(%with-viewport (attachment-viewport (%attachment ,fbo ,attachment))
     ,@body))
