(in-package :cgl)

;; This has many lookup vars which a quite ugly and will need to be 
;; consolidated with other such tables in cepl. For now I am still hashing
;; out the feel of this library so this is scheduled for once I can be
;; sure that this approach will feel right

;;--------------------------------------------------------------
;; PIXEL FORMAT
;;--------------
;; [TODO] Need 3rd option for normalised?..:na for floats
;; [TODO] Add guaranteed flags to formats
;; [TODO] add half float
;; [TODO] add :stencil-only
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
(defparameter *gl-pixel-to-internal-map*
  '(((:DEPTH t :short nil) :DEPTH-COMPONENT16)
    ((:DEPTH t :int nil) :DEPTH-COMPONENT32)
    ((:DEPTH t :float nil) :DEPTH-COMPONENT32F)
    ((:STENCIL-ONLY t :int nil) :STENCIL-INDEX8)
    ((:R t :ubyte nil) :R8)
    ((:R t :byte nil) :R8-SNORM)
    ((:R t :ushort nil) :R16)
    ((:R t :short nil) :R16-SNORM)
    ((:RG t :ubyte nil) :RG8)
    ((:RG t :byte nil) :RG8-SNORM)
    ((:RG t :ushort nil) :RG16)
    ((:RG t :short nil) :RG16-SNORM)
    ((:RGB t :ubyte nil) :RGB8)
    ((:RGB t :byte nil) :RGB8-SNORM)
    ((:RGB t :short nil) :RGB16-SNORM)
    ((:RGBA t :ubyte nil) :RGBA8)
    ((:RGBA t :byte nil) :RGBA8-SNORM)
    ((:RGBA t :ushort nil) :RGBA16)
    ((:R t :float nil) :R32F)
    ((:RG t :float nil) :RG32F)
    ((:RGB t :float nil) :RGB32F)
    ((:RGBA t :float nil) :RGBA32F)
    ((:R nil :byte nil) :R8I)
    ((:R nil :ubyte nil) :R8UI)
    ((:R nil :short nil) :R16I)
    ((:R nil :ushort nil) :R16UI)
    ((:R nil :int nil) :R32I)
    ((:R nil :uint nil) :R32UI)
    ((:RG nil :byte nil) :RG8I)
    ((:RG nil :ubyte nil) :RG8UI)
    ((:RG nil :short nil) :RG16I)
    ((:RG nil :ushort nil) :RG16UI)
    ((:RG nil :int nil) :RG32I)
    ((:RG nil :uint nil) :RG32UI)
    ((:RGB nil :byte nil) :RGB8I)
    ((:RGB nil :ubyte nil) :RGB8UI)
    ((:RGB nil :short nil) :RGB16I)
    ((:RGB nil :ushort nil) :RGB16UI)
    ((:RGB nil :int nil) :RGB32I)
    ((:RGB nil :uint nil) :RGB32UI)
    ((:RGBA nil :byte nil) :RGBA8I)
    ((:RGBA nil :ubyte nil) :RGBA8UI)
    ((:RGBA nil :short nil) :RGBA16I)
    ((:RGBA nil :ushort nil) :RGBA16UI)
    ((:RGBA nil :int nil) :RGBA32I)
    ((:RGBA nil :uint nil) :RGBA32UI)
    ((:RGB t :ubyte (8 8 8)) :SRGB8)
    ((:RGBA t :ubyte (8 8 8 8)) :SRGB8-ALPHA8)
    ((:RGBA t :uint (10 10 10 2)) :RGB10-A2)
    ((:RGBA nil :uint (10 10 10 2)) :RGB10-A2UI)
    ((:RGB t :ubyte (2 2 2 2)) :RGBA2)
    ((:RGB t :ushort (4 4 4 4)) :RGBA4)
    ((:RGBA t :short (5 5 5 1)) :RGB5-A1)
    ((:RGB t :ubyte (3 3 2)) :R3-G3-B2)))

;; [TODO] Does this need to be a structure? could it be a list with the 
;;        we could have a compiler macro to eval this at compile time,
;;        this would mean other functions could transform and inline this
;;        value
(defstruct pixel-format
  components type normalise sizes reversed comp-length)

(defun describe-pixel-format (object)
  (let ((pf (if (cgl::pixel-format-p object) object (pixel-format-of object))))
    (print "---------------")
    (when pf
      (print pf)
      (let ((cf (cgl::compile-pixel-format pf)))
        (format t "~%format: ~s~%type: ~s" (first cf) (second cf)))
      (format t "~%internalFormat: ~s" (cgl:internal-format-from-pixel-format pf)))
    (print "---------------"))
  t)

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
              (and (eql component-length (length type))
                   (or (assoc (if reversed (cons :r type) type)
                              *valid-pixel-packed-sizes* :test #'equal)
                       '(nil nil))))
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

(defun pixel-format (components &optional (type :ubyte) (normalise t) reversed)
  (destructuring-bind 
        (components type sizes normalise reversed component-length)
      (process-pixel-format components type normalise reversed)
    (make-pixel-format :components components :type type 
                       :sizes (if reversed (rest sizes) sizes)
                       :normalise normalise :reversed reversed
                       :comp-length component-length)))

;; [TODO] swap intern for utils:kwd
(defun compile-pixel-format (pixel-format)
  (let* ((components (pixel-format-components pixel-format))
         (components (if (eq components :depth) :depth-component components))
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
;; INTERNAL-FORMATS
;;------------------

(defun internal-format-from-pixel-format (pixel-format)
  (second (assoc (list (pixel-format-components pixel-format)
                       (pixel-format-normalise pixel-format)
                       (pixel-format-type pixel-format)
                       (pixel-format-sizes pixel-format)) 
                 *gl-pixel-to-internal-map*
                 :test #'equal)))

;; [TODO] REVERSED??
(defun pixel-format-from-internal-format (internal-format)
  (destructuring-bind (components normalise type sizes)
      (first (rassoc internal-format *gl-pixel-to-internal-map*
                     :key #'car :test #'eq))
    (make-pixel-format
     :components components :type type :normalise normalise
     :sizes sizes :reversed nil
     :comp-length (get-component-length components))))


;;--------------------------------------------------------------
;; LOOKUPS
;;---------

(defmethod pixel-format-of ((type t))
  (when (find type *valid-pixel-types*)
    (pixel-format :r type)))
