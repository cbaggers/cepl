(in-package :cgl)
(named-readtables:in-readtable fn_::fn_lambda)

;;------------------------------------------------------------
;; system

(defun fill-swatch-with-tiles (swatch)
  (let* ((tex (slot-value swatch 'texture))
         (d (base-dimensions tex))
         (tex-array (texref tex))
         (col-a (make-array 4 :initial-contents (list 50 50 50 1)))
         (col-b (make-array 4 :initial-contents (list 128 128 128 1))))
    (gl-push 
     (loop :for x :below (first d) :collect
        (loop :for y :below (second d) :collect
           (let ((x (mod x 2)) (y (mod y 2)))
             (if (= x y) col-a col-b))))
     tex-array)
    swatch))

;;------------------------------------------------------------
;; class and construction

(defclass swatch () 
  ((pos :initform (v! 0 0) :initarg :pos)
   (size :initform (v! 0.1 0.1) :initarg :size)
   (texture :initform (make-texture nil :dimensions '(100 100)
                                   :internal-format :rgba8)
            :initarg :texture)
   (fbo :initform (make-fbo))
   (attachment :initform :color-attachment0 :initarg :attachment)))

(defmethod size ((swatch swatch)) (slot-value swatch 'size))
(defmethod (setf size) (val (swatch swatch)) 
  (setf (slot-value swatch 'size) (v2:v* val 2.0)))

(defmethod pos ((swatch swatch)) (slot-value swatch 'pos))
(defmethod (setf pos) (val (swatch swatch)) 
  (setf (slot-value swatch 'pos) 
        (v2:v-1 (v2:v* val 2.0) (v! 1 1))))

(defmethod initialize-instance :after ((swatch swatch) &key)
  (with-slots (texture fbo attachment) swatch
    (fbo-attach fbo (texref texture) attachment)))


(defparameter *swatch-attachments* 
  '((:color . :color-attachment0) (:depth . :depth-attachment)))
(defparameter *swatch-texture-formats* 
  '((:color . :rgba8) (:depth . :depth-component16)))

(defun lookup-attachment (x)
  (or (cdr (assoc x *swatch-attachments*))
      (error "suitable swatch attachment for ~s not found, options are ~s" 
             x (mapcar #'car *swatch-attachments*))))
(defun lookup-texture-format (x)
  (or (cdr (assoc x *swatch-texture-formats*))
      (error "suitable swatch texture format ~s not found, options are ~s" 
             x (mapcar #'car *swatch-texture-formats*))))

(defun make-swatch (&key (size (v! 0.1 0.1)) (tex-size '(320 240))
                      (attachment :color))
  (let ((x (make-instance 'swatch :texture 
                          (make-texture nil :dimensions tex-size
                                        :internal-format 
                                        (lookup-texture-format attachment))
                          :attachment (lookup-attachment attachment))))
    (setf (pos x) (v! 0 0))
    (setf (size x) (v! (aref size 0) (aref size 1)))
    ;;(fill-swatch-with-tiles x)
    x))

;;------------------------------------------------------------
;; drawing

(defun draw-swatch (swatch)
  (with-slots (pos size texture attachment) swatch
    (gl:disable :depth-test)
    (if (eq attachment :depth-attachment)
        (gmap #'swatch-depth-renderer *quad-stream*
              :tex texture
              :offset pos
              :scale size)
        (gmap #'swatch-renderer *quad-stream*
              :tex texture
              :offset pos
              :scale size))
    (gl:enable :depth-test)
    t))

(defpipeline swatch-renderer ((data :vec2) &uniform (offset :vec2)
                              (tex :sampler-2d) (scale :vec2))
  (:vertex (setf gl-position (v! (+ (* data scale) offset) 1 1))
           (out tex-coord data))
  (:fragment (out output-color (texture tex tex-coord))))

(defpipeline swatch-depth-renderer ((data :vec2) &uniform (offset :vec2)
                              (tex :sampler-2d) (scale :vec2))
  (:vertex (setf gl-position (v! (+ (* data scale) offset) 1 1))
           (out tex-coord data))
  (:fragment (let ((col (texture tex tex-coord)))
               (out output-color (v! (x col) (x col) (x col) 1)))))


;;------------------------------------------------------------
;; render to swatch

(defmacro with-swatch-bound ((swatch &optional (bound-to :color-buffer))
                             &body body)  
  (declare (ignore bound-to))
  `(with-bind-fbo ((slot-value ,swatch 'fbo) :framebuffer)
     ,@body))
