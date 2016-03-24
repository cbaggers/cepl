(in-package :cepl.pixel-formats)

;; This has many lookup vars which a quite ugly and will need to be
;; consolidated with other such tables in cepl. For now I am still hashing
;; out the feel of this library so this is scheduled for once I can be
;; sure that this approach will feel right

;;--------------------------------------------------------------
;; Pixel Format
;;--------------
;; The pixel format struct is a more explorable representation of the
;; internal-format of gl textures. Also they also help with the ugliness
;; of the texture api, where different parts want the texture information
;; in different ways. For example when defining a texture to hold :uint8's the
;; system calls the format :R8, but when uploading data to the texture it
;; wants the format specified as :RED :UNSIGNED-BYTE.

;; There are also so many combinations of this crap that memorizing it all is
;; too damn hard. So we have conversion functions to switch between a subset of
;; the permutations. It's a subset as not everything maps 1 to 1

;;--------------------------------------------------------------

;; [TODO] Need 3rd option for normalised?..:na for floats
;; [TODO] Add guaranteed flags to formats
;; [TODO] add half float
;; [TODO] add :stencil-only
(defvar *valid-pixel-components*
  '(:r :g :b :rg :rgb :rgba :bgr :bgra :depth :depth-stencil))

(defvar *valid-pixel-types*
  '(:uint8 :int8 :ushort :short :uint :int :float))

(defvar *valid-pixel-packed-sizes*
  '(((3 3 2) :uint8) ((:r 2 3 3) :uint8)
    ((5 6 5) :ushort) ((:r 5 6 5) :ushort)
    ((4 4 4 4) :ushort) ((:r 4 4 4 4) :ushort)
    ((5 5 5 1) :ushort) ((:r 1 5 5 5) :ushort)
    ((8 8 8 8) :uint) ((:r 8 8 8 8) :uint)
    ((10 10 10 2) :uint) ((:r 2 10 10 10) :uint)
    ((24 8) :uint) ((:r 10 11 11) :uint) ((:r 5 9 9 9) :uint)))

(defvar *gl-integral-pixel-types*
  '(:uint8 :int8 :ushort :short :uint :int))

(defparameter *gl-pixel-to-internal-map*
  '(((:depth t :short nil) :depth-component16)
    ((:depth t :int nil) :depth-component32)
    ((:depth t :float nil) :depth-component32f)
    ((:stencil-only t :int nil) :stencil-index8)
    ((:r t :uint8 nil) :r8)
    ((:r t :int8 nil) :r8-snorm)
    ((:r t :ushort nil) :r16)
    ((:r t :short nil) :r16-snorm)
    ((:rg t :uint8 nil) :rg8)
    ((:rg t :int8 nil) :rg8-snorm)
    ((:rg t :ushort nil) :rg16)
    ((:rg t :short nil) :rg16-snorm)
    ((:rgb t :uint8 nil) :rgb8)
    ((:rgb t :int8 nil) :rgb8-snorm)
    ((:rgb t :short nil) :rgb16-snorm)
    ((:rgba t :uint8 nil) :rgba8)
    ((:rgba t :int8 nil) :rgba8-snorm)
    ((:rgba t :ushort nil) :rgba16)
    ((:r t :float nil) :r32f)
    ((:rg t :float nil) :rg32f)
    ((:rgb t :float nil) :rgb32f)
    ((:rgba t :float nil) :rgba32f)
    ((:r t :float nil) :r16f)
    ((:rg t :float nil) :rg16f)
    ((:rgb t :float nil) :rgb16f)
    ((:rgba t :float nil) :rgba16f)
    ((:r nil :int8 nil) :r8i)
    ((:r nil :uint8 nil) :r8ui)
    ((:r nil :short nil) :r16i)
    ((:r nil :ushort nil) :r16ui)
    ((:r nil :int nil) :r32i)
    ((:r nil :uint nil) :r32ui)
    ((:rg nil :int8 nil) :rg8i)
    ((:rg nil :uint8 nil) :rg8ui)
    ((:rg nil :short nil) :rg16i)
    ((:rg nil :ushort nil) :rg16ui)
    ((:rg nil :int nil) :rg32i)
    ((:rg nil :uint nil) :rg32ui)
    ((:rgb nil :int8 nil) :rgb8i)
    ((:rgb nil :uint8 nil) :rgb8ui)
    ((:rgb nil :short nil) :rgb16i)
    ((:rgb nil :ushort nil) :rgb16ui)
    ((:rgb nil :int nil) :rgb32i)
    ((:rgb nil :uint nil) :rgb32ui)
    ((:rgba nil :int8 nil) :rgba8i)
    ((:rgba nil :uint8 nil) :rgba8ui)
    ((:rgba nil :short nil) :rgba16i)
    ((:rgba nil :ushort nil) :rgba16ui)
    ((:rgba nil :int nil) :rgba32i)
    ((:rgba nil :uint nil) :rgba32ui)
    ((:rgb t :uint8 (8 8 8)) :srgb8)
    ((:rgba t :uint8 (8 8 8 8)) :srgb8-alpha8)
    ((:rgba t :uint (10 10 10 2)) :rgb10-a2)
    ((:rgba nil :uint (10 10 10 2)) :rgb10-a2ui)
    ((:rgb t :uint8 (2 2 2 2)) :rgba2)
    ((:rgb t :ushort (4 4 4 4)) :rgba4)
    ((:rgba t :short (5 5 5 1)) :rgb5-a1)
    ((:rgb t :uint8 (3 3 2)) :r3-g3-b2)))

(defstruct pixel-format
  components type normalise sizes reversed comp-length)

(defun describe-pixel-format (object)
  (let ((pf (if (pixel-format-p object)
                object
                (lisp-type->pixel-format object))))
    (print "---------------")
    (when pf
      (print pf)
      (let ((cf (compile-pixel-format pf)))
        (format t "~%format: ~s~%type: ~s" (first cf) (second cf)))
      (format t "~%internalFormat: ~s" (pixel-format->internal-format pf)))
    (print "---------------"))
  t)

(defun describe-internal-format (format)
  (describe-pixel-format (internal-format->pixel-format format)))

(defun get-component-length (components)
  (case components
    (:depth 1) (:depth-stencil 2)
    (t (length (symbol-name components)))))

(defun valid-pixel-format-p (components type normalise reversed)
  (let ((component-length (get-component-length components)))
    (when (and (find components *valid-pixel-components*)
               (if (listp type) (eql component-length (length type)) t))
      (destructuring-bind (sizes type)
          (if (keywordp type)
              (list nil (find type *valid-pixel-types*))
              (if (symbolp type)
                  '(nil nil)
                  (and (eql component-length (length type))
                       (or (assoc (if reversed (cons :r type) type)
                                  *valid-pixel-packed-sizes* :test #'equal)
                           '(nil nil)))))
        (when (and type (not (and (not normalise)
                                  (not (find type *gl-integral-pixel-types*)))))
          (list components type (if reversed (rest sizes) sizes)
                normalise reversed component-length))))))

(defun process-pixel-format (components type normalise reversed)
  (unless (find components *valid-pixel-components*)
    (error "Not a valid pixel component layout.~%~s not found in '~s"
           components *valid-pixel-components*))
  (let ((component-length (get-component-length components)))
    (when (listp type) (unless (eql component-length (length type))
                         (error "Number of sizes and components do not match")))
    (destructuring-bind (sizes type)
        (if (keywordp type)
            (list nil (find type *valid-pixel-types*))
            (and (eql component-length (length type))
                 (or (assoc (if reversed (cons :r type) type)
                            *valid-pixel-packed-sizes* :test #'equal)
                     '(nil nil))))
      (unless type (error "Not a known pixel type: <components:~a type:~a>"
                          components type))
      (when (and (not normalise) (not (find type *gl-integral-pixel-types*)))
        (error "The type ~a cannot hold un-normalised integers" type))
      (list components type (if reversed (rest sizes) sizes)
            normalise reversed component-length))))

(defun pixel-format (components &optional (type :uint8) (normalise t) reversed)
  (destructuring-bind
        (components type sizes normalise reversed component-length)
      (process-pixel-format components type normalise reversed)
    (make-pixel-format :components components :type type
                       :sizes (if reversed (rest sizes) sizes)
                       :normalise normalise :reversed reversed
                       :comp-length component-length)))

;; [TODO] swap intern for cepl-utils:kwd
(defun compile-pixel-format (pixel-format)
  (let* ((components (pixel-format-components pixel-format))
         (components (if (eq components :depth) :depth-component components))
         (gl-comps (or (rest (assoc components '((:r . :red) (:g . :green)
                                                 (:b . :blue))))
                       components))
         (sizes (pixel-format-sizes pixel-format))
         (type (pixel-format-type pixel-format))
         (expanded-type (expand-gl-type-name type)))
    (let ((format (if (pixel-format-normalise pixel-format)
                      gl-comps
                      (intern (format nil "~a-INTEGER" gl-comps) 'keyword)))
          (type (if sizes
                    (intern (format nil "~a~{-~a~}~@[-REV~]" expanded-type sizes
                                    (pixel-format-reversed pixel-format))
                            'keyword)
                    expanded-type)))
      (list format type))))

(defun pixel-format->lisp-type (pixel-format)
  (if (pixel-format-sizes pixel-format)
      (pixel-format-type pixel-format)
      (let ((len (pixel-format-comp-length pixel-format))
            (type (pixel-format-type pixel-format)))
        (values (if (> len 1)
                    (intern (format nil "~@[~a-~]VEC~a"
                                    (unless (eq type :float) type)
                                    len) 'keyword)
                    type)))))

(defun internal-format->lisp-type (internal-format)
  (let ((pformat (internal-format->pixel-format internal-format)))
    (if pformat
        (pixel-format->lisp-type pformat)
        (error 'internal-format->lisp-type-failed
               :type-name internal-format))))

(defun lisp-type->internal-format (lisp-type)
  (let ((pformat (lisp-type->pixel-format lisp-type)))
    (or (pixel-format->internal-format pformat :error-if-missing nil)
        (error 'lisp-type->internal-format-failed
               :type-name lisp-type))))

;;--------------------------------------------------------------
;; Internal-Formats
;;------------------

(defun pixel-format->internal-format
    (pixel-format &key (error-if-missing t))
  (let ((result (second (assoc (list (pixel-format-components pixel-format)
                                     (pixel-format-normalise pixel-format)
                                     (pixel-format-type pixel-format)
                                     (pixel-format-sizes pixel-format))
                               *gl-pixel-to-internal-map*
                               :test #'equal))))
    (or result
        (when error-if-missing
          (error 'pixel-format->internal-format-failed
                 :type-name pixel-format)))))

;; [TODO] REVERSED??
(defun internal-format->pixel-format
    (internal-format &key (error-if-missing t))
  (let ((pf (first (rassoc internal-format *gl-pixel-to-internal-map*
                           :key #'car :test #'eq))))
    (if pf
        (destructuring-bind (components normalise type sizes)
            pf
          (make-pixel-format
           :components components :type type :normalise normalise
           :sizes sizes :reversed nil
           :comp-length (get-component-length components)))
        (when error-if-missing
          (error 'internal-format->pixel-format-failed
                 :type-name internal-format)))))


;;--------------------------------------------------------------
;; Lisp Types
;;------------

(defmethod lisp-type->pixel-format ((type t))
  (when (find type *valid-pixel-types*)
    (pixel-format :r type)))
