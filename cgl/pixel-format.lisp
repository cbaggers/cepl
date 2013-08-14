(in-package :cgl)

;; This has many lookup vars which a quite ugly and will need to be 
;; consolidated with other such tables in cepl. For now I am still hashing
;; out the feel of this library so this is scheduled for once I can be
;; sure that this approach will feel right

;;--------------------------------------------------------------
;; PIXEL FORMAT
;;--------------
;; [TODO] Add guaranteed flags to formats
;; [TODO] add half float
;; [TODO] add stencil-only
(defparameter *valid-pixel-components*
  '(:r :g :b :rg :rgb :rgba :bgr :bgra :depth :depth-stencil))
(defparameter *valid-pixel-types* 
  '(:ubyte :byte :ushort :short :uint :int :float))
(defparameter *valid-pixel-packed-sizes* 
  '(((3 3 2) :ubyte) ((:r 2 3 3) :ubyte) 
    ((5 6 5) :ushort) ((:r 5 6 5) :ushort)
    ((4 4 4 4) :ushort) ((:r 4 4 4 4) :ushort)
    ((5 5 5 1) :ushort) ((:r 1 5 5 5) :ushort)
    ((8 8 8 8) :uint) ((:r 8 8 8 8) :uint)
    ((10 10 10 2) :uint) ((:r 2 10 10 10) :uint)
    ((24 8) :uint) ((:r 10 11 11) :uint) ((:r 5 9 9 9) :uint)))
(defparameter *gl-integral-pixel-types* 
  '(:ubyte :byte :ushort :short :uint :int))
(defparameter *expanded-gl-type-names* 
  '((:uint :unsigned-int) (:ubyte :unsigned-byte)
    (:ubyte :unsigned-byte) (:ushort :unsigned-short)))

;; [TODO] Does this need to be a structure? could it be a list with the 
;;        we could have a compiler macro to eval this at compile time,
;;        this would mean other functions could transform and inline this
;;        value
(defstruct pixel-format
  components type normalise sizes reversed comp-length)

;; [TODO] byte and ubyte to cffi 
(defun pixel-format (components &optional (type :ubyte) (normalise t) reversed)
  (unless (find components *valid-pixel-components*)
    (error "Not a valid pixel component layout.~%~s not found in '~s"
           components *valid-pixel-components*))
  (let ((component-length (case components 
                            (:depth 1) (:depth-stencil 2) 
                            (t (length (symbol-name components))))))
    (when (listp type) (unless (eql component-length (length type))
                         (error "Number of sizes and components do not match")))
    (destructuring-bind (sizes type)
        (if (keywordp type)
            (list nil (find type *valid-pixel-types*))
            (and (eql component-length (length type))
                 (or (assoc (if reversed (cons :r type) type)
                            *valid-pixel-packed-sizes* :test #'equal)
                     '(nil nil))))
      (unless type (error "Not a know pixel type: <components:~a type:~a>"
                          components type))
      (when (and (not normalise) (not (find type *gl-integral-pixel-types*)))
        (error "The type ~a cannot hold un-normalised integers" type))
      (make-pixel-format :components components :type type 
                         :sizes (if reversed (rest sizes) sizes)
                         :normalise normalise :reversed reversed
                         :comp-length component-length))))

;; [TODO] swap intern for utils:kwd
(defun compile-pixel-format (pixel-format)
  (let* ((components (pixel-format-components pixel-format))
         (gl-comps (or (rest (assoc components '((:r . :red) (:g . :green) 
                                                 (:b . :blue))))
                       components))
         (sizes (pixel-format-sizes pixel-format))
         (type (pixel-format-type pixel-format))
         (expanded-type (or (second (assoc type *expanded-gl-type-names*)) 
                            type)))
    (let ((format (if (pixel-format-normalise pixel-format)
                      gl-comps
                      (intern (format nil "~a-INTEGER" gl-comps) 'keyword)))
          (type (if sizes
                    (intern (format nil "~a~{-~a~}~@[-REV~]" expanded-type sizes
                                    (pixel-format-reversed pixel-format)) 
                            'keyword)
                    expanded-type)))
      (list format type))))

(defun pixel-format-element-type (pixel-format)
  (if (pixel-format-sizes pixel-format)
      (pixel-format-type pixel-format)
      (let ((len (pixel-format-comp-length pixel-format))
            (type (pixel-format-type pixel-format)))
        (values (if (> len 1)
                    (intern (format nil "~@[~a-~]VEC~a" 
                                    (unless (eq type :float) type)
                                    len) 'keyword)
                    type)))))

;;--------------------------------------------------------------


;; [NOTE] This is damn interesting
;; The format​ parameter of a pixel transfer function defines the following:
;; The basic type of data that is being read/written from/to: 
;; Color, depth, stencil, or depth/stencil. 
;; This must match the image format of the image being read/written from/to.

;;; 
;; (defun pixel-image-formats-compatible-p (pixel-format image-format)
;;   (when (eq image-format :depth-component)
;;     (unless (find pixel-format '(:depth-component :depth-component16
;;                                  :depth-component24 :depth-component32f))
;;       (error "If the pixel data is of type :depth-component then the destination
;; must also be a depth type instead of ~a" pixel-format)))
;;   (when (find pixel-format '(:depth-component :depth-component16
;;                                  :depth-component24 :depth-component32f))
;;     (unless (eq image-format :depth-component)
;;       (error "If the destination is a depth gpu-array then the pixel data
;; must have a type of :depth-component instead of ~a" image-format)))
;;   (when (find image-format '())
;;     (unless (find pixel-format '())
;;       (error )))
;;   t)
