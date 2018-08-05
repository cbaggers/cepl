(in-package :cepl.pixel-formats)

;; This has many lookup vars which a quite ugly and will need to be
;; consolidated with other such tables in cepl. For now I am still hashing
;; out the feel of this library so this is scheduled for once I can be
;; sure that this approach will feel right

;;--------------------------------------------------------------
;; Pixel Format
;;--------------
;; The pixel format struct is a more explorable representation of the
;; image-format of gl textures. Also they also help with the ugliness
;; of the texture api, where different parts want the texture information
;; in different ways. For example when defining a texture to hold :uint8's the
;; system calls the format :R8, but when uploading data to the texture it
;; wants the format specified as :RED :UNSIGNED-BYTE.

;; There are also so many combinations of this crap that memorizing it all is
;; too damn hard. So we have conversion functions to switch between a subset of
;; the permutations. It's a subset as not everything maps 1 to 1

;;--------------------------------------------------------------

;; [TODO] Need 3rd option for normalized?..:na for floats
;; [TODO] Add guaranteed flags to formats
;; [TODO] add half float
;; [TODO] add :stencil-only
(define-const +valid-pixel-components+
    '(:r :g :b :rg :rgb :rgba :bgr :bgra :depth :depth-stencil)
  :type list)

(define-const +valid-pixel-types+
    '(:uint8 :int8 :ushort :short :uint :int :float :half-float)
  :type list)

(define-const +valid-pixel-packed-sizes+
    '(((3 3 2) :uint8) ((:r 2 3 3) :uint8)
      ((5 6 5) :ushort) ((:r 5 6 5) :ushort)
      ((4 4 4 4) :ushort) ((:r 4 4 4 4) :ushort)
      ((5 5 5 1) :ushort) ((:r 1 5 5 5) :ushort)
      ((8 8 8 8) :uint) ((:r 8 8 8 8) :uint)
      ((10 10 10 2) :uint) ((:r 2 10 10 10) :uint)
      ((24 8) :uint) ((:r 10 11 11) :uint) ((:r 5 9 9 9) :uint))
  :type list)

(define-const +gl-integral-pixel-types+
    '(:uint8 :int8 :ushort :short :uint :int)
  :type list)

(define-const +gl-pixel-to-internal-map+
    ;; (components normalize type sizes)
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
      ((:r t :half-float nil) :r16f)
      ((:rg t :half-float nil) :rg16f)
      ((:rgb t :half-float nil) :rgb16f)
      ((:rgba t :half-float nil) :rgba16f)
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
      ((:rgb t :uint8-vec3 (8 8 8)) :srgb8)
      ((:rgba t :uint8-vec4 (8 8 8 8)) :srgb8-alpha8)
      ((:rgba t :uint (10 10 10 2)) :rgb10-a2)
      ((:rgba nil :uint (10 10 10 2)) :rgb10-a2ui)
      ((:rgb t :uint8 (2 2 2 2)) :rgba2) ;;bug? rgb v rgba?
      ((:rgb t :ushort (4 4 4 4)) :rgba4) ;;bug? rgb v rgba?
      ((:rgba t :short (5 5 5 1)) :rgb5-a1)
      ((:rgb t :uint8 (3 3 2)) :r3-g3-b2))
  :type list)

(defun+ describe-pixel-format (object)
  (let ((pf (if (pixel-format-p object)
                object
                (lisp-type->pixel-format object))))
    (print "---------------")
    (when pf
      (print pf)
      (let ((cf (compile-pixel-format pf)))
        (format t "~%format: ~s~%type: ~s" (first cf) (second cf)))
      (format t "~%internalFormat: ~s" (pixel-format->image-format pf)))
    (print "---------------"))
  t)

(defun+ describe-image-format (format)
  (describe-pixel-format (image-format->pixel-format format)))

(defun+ get-component-length (components)
  (case components
    (:depth 1) (:depth-stencil 2)
    (t (length (symbol-name components)))))

(defun+ valid-pixel-format-p (components type normalize reversed)
  (let ((component-length (get-component-length components)))
    (when (and (find components +valid-pixel-components+)
               (if (listp type) (eql component-length (length type)) t))
      (destructuring-bind (sizes type)
          (if (keywordp type)
              (list nil (find type +valid-pixel-types+))
              (if (symbolp type)
                  '(nil nil)
                  (and (eql component-length (length type))
                       (or (assoc (if reversed (cons :r type) type)
                                  +valid-pixel-packed-sizes+ :test #'equal)
                           '(nil nil)))))
        (when (and type (not (and (not normalize)
                                  (not (find type +gl-integral-pixel-types+)))))
          (list components type (if reversed (rest sizes) sizes)
                normalize reversed component-length))))))

(defun+ process-pixel-format (components type normalize reversed)
  (unless (find components +valid-pixel-components+)
    (error "Not a valid pixel component layout.~%~s not found in '~s"
           components +valid-pixel-components+))
  (let ((component-length (get-component-length components)))
    (when (listp type) (unless (eql component-length (length type))
                         (error "Number of sizes and components do not match")))
    (destructuring-bind (sizes type)
        (if (keywordp type)
            (list nil (find type +valid-pixel-types+))
            (and (eql component-length (length type))
                 (or (assoc (if reversed (cons :r type) type)
                            +valid-pixel-packed-sizes+ :test #'equal)
                     '(nil nil))))
      (unless type (error "Not a known pixel type: <components:~a type:~a>"
                          components type))
      (when (and (not normalize) (not (find type +gl-integral-pixel-types+)))
        (error "The type ~a cannot hold un-normalized integers" type))
      (list components type (if reversed (rest sizes) sizes)
            normalize reversed component-length))))

(defun+ pixel-format! (components &optional (type :uint8) (normalize t) reversed)
  (destructuring-bind
        (components type sizes normalize reversed component-length)
      (process-pixel-format components type normalize reversed)
    (make-pixel-format :components components :type type
                       :sizes (if reversed (rest sizes) sizes)
                       :normalize normalize :reversed reversed
                       :comp-length component-length)))

;; [TODO] swap intern for cepl-utils:kwd
(defn compile-pixel-format ((pixel-format pixel-format)) list
  (let* ((components (pixel-format-components pixel-format))
         (components (if (eq components :depth) :depth-component components))
         (gl-comps (or (rest (assoc components '((:r . :red) (:g . :green)
                                                 (:b . :blue))))
                       components))
         (sizes (pixel-format-sizes pixel-format))
         (type (pixel-format-type pixel-format))
         (expanded-type (cffi-type->gl-type (if (eq type :int8) :uint8 type))))
    (let ((format (if (pixel-format-normalize pixel-format)
                      gl-comps
                      (intern (format nil "~a-INTEGER" gl-comps) 'keyword)))
          (type (if sizes
                    (intern (format nil "~a~{-~a~}~@[-REV~]" expanded-type sizes
                                    (pixel-format-reversed pixel-format))
                            'keyword)
                    expanded-type)))
      (list format type))))

(defun+ pixel-format->lisp-type (pixel-format)
  (if (pixel-format-sizes pixel-format)
      (pixel-format-type pixel-format)
      (let ((len (pixel-format-comp-length pixel-format))
            (type (pixel-format-type pixel-format)))
        ;; {TODO} this approach is terrible, replace it
        (values (if (> len 1)
                    (intern
                     (format nil "~@[~a-~]VEC~a"
                             (case type
                               (:float nil)
                               (:half-float :half)
                               (otherwise type))
                             len)
                     'keyword)
                    type)))))

(defun+ image-format->lisp-type (image-format)
  (let ((pformat (image-format->pixel-format image-format)))
    (if pformat
        (pixel-format->lisp-type pformat)
        (error 'image-format->lisp-type-failed
               :type-name image-format))))

(defun+ lisp-type->image-format (lisp-type)
  (let ((pformat (lisp-type->pixel-format lisp-type)))
    (or (and pformat
             (pixel-format->image-format pformat :error-if-missing nil))
        (error 'lisp-type->image-format-failed
               :type-name lisp-type))))

;;--------------------------------------------------------------
;; Image-Formats
;;------------------

(defun+ pixel-format->image-format (pixel-format &key (error-if-missing t))
  (let ((result (second (assoc (list (pixel-format-components pixel-format)
                                     (pixel-format-normalize pixel-format)
                                     (pixel-format-type pixel-format)
                                     (pixel-format-sizes pixel-format))
                               +gl-pixel-to-internal-map+
                               :test #'equal))))
    (or result
        (when error-if-missing
          (error 'pixel-format->image-format-failed
                 :type-name pixel-format)))))

;; [TODO] REVERSED??
(defun+ image-format->pixel-format
    (image-format &key (error-if-missing t))
  (let ((pf (first (rassoc image-format +gl-pixel-to-internal-map+
                           :key #'car :test #'eq))))
    (if pf
        (destructuring-bind (components normalize type sizes)
            pf
          (make-pixel-format
           :components components :type type :normalize normalize
           :sizes sizes :reversed nil
           :comp-length (get-component-length components)))
        (when error-if-missing
          (error 'image-format->pixel-format-failed
                 :type-name image-format)))))


;;--------------------------------------------------------------
;; Lisp Types
;;------------

(defmethod lisp-type->pixel-format ((type t))
  (when (find type +valid-pixel-types+)
    (cepl.pixel-formats::pixel-format! :r type)))
