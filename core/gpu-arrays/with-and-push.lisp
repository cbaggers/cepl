(in-package :cepl.gpu-arrays)

(defmacro with-gpu-array-as-pointer
    ((temp-name gpu-array &key (access-type :read-write) (target :array-buffer))
     &body body)
  "This macro is really handy if you need to have random access
   to the data on the gpu. It takes a gpu-array and maps it
   giving you the pointer"
  (assert (keywordp target))
  (unless (find access-type '(:read-write :read-only :write-only))
    (error "The access argument must be set to :read-write :read-only or :write-only"))
  (let ((glarray-pointer (gensym "POINTER"))
        (array-sym (gensym "BUFFER"))
        (buffer-sym (gensym "BUFFER"))
        (gtarget (gensym "target")))
    (labels ((tgt () (if (keywordp target) target gtarget)))
      `(progn
         (let ((,array-sym ,gpu-array))
           (unless (typep ,array-sym 'gpu-array)
             (if (typep ,array-sym 'gpu-array-t)
                 (error "Unfortunately cepl doesnt not support texture backed gpu-array right now, it should, and it will...But not today. Prod me with a github issue if you need this urgently")
                 (error "with-gpu-array-* does not support the type ~s"
                        (type-of ,array-sym))))
           (let ((,buffer-sym (gpu-array-buffer ,array-sym))
                 ,@(unless (keywordp target) `((,gtarget ,target))))
             (setf (cepl.context:gpu-buffer-bound (cepl.context:cepl-context) ,(tgt)) ,buffer-sym)
             (gl:with-mapped-buffer (,glarray-pointer
                                     ,(tgt)
                                     ,access-type)
               (if (pointer-eq ,glarray-pointer (null-pointer))
                   (error "with-gpu-array-as-*: buffer mapped to null pointer~%Have you defintely got an opengl context?~%~s"
                          ,glarray-pointer)
                   (let ((,temp-name ,glarray-pointer))
                     ,@body)))))))))

(defmacro with-gpu-array-as-c-array
    ((temp-name gpu-array &key (access-type :read-write)) &body body)
  (let ((ggpu-array (gensym "gpu-array")))
    `(let ((,ggpu-array ,gpu-array))
       (with-gpu-array-as-pointer (,temp-name ,ggpu-array :access-type ,access-type)
         (let ((,temp-name
                (make-c-array-from-pointer
                 (gpu-array-dimensions ,ggpu-array)
                 (element-type ,ggpu-array)
                 (cffi:inc-pointer
                  ,temp-name
                  (gpu-array-bb-offset-in-bytes-into-buffer ,ggpu-array)))))
           ,@body)))))


(defmethod print-mem ((thing gpu-array-bb)
                      &optional
                        (size-in-bytes 64)
                        (offset 0))
  (with-gpu-array-as-pointer (a thing :access-type :read-only)
    (print-mem (cffi:inc-pointer a offset) size-in-bytes)))



(defn-inline gpu-array-element-type ((gpu-array gpu-array)) symbol
  (declare (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 0))
           (profile t))
  (etypecase gpu-array
    (gpu-array-t
     (%cepl.types::gpu-array-t-image-format gpu-array))
    (gpu-array-bb
     (cepl.gpu-arrays.buffer-backed::gpu-array-bb-element-type gpu-array))))

(defun+ backed-by (gpu-array)
  (etypecase gpu-array
    (gpu-array-t :texture)
    (gpu-array-bb :buffer)))



(defn copy-buffer-backed-gpu-array-to-new-c-array ((src gpu-array-bb))
    c-array
  (with-gpu-array-as-c-array (x src :access-type :read-only)
    (clone-c-array x)))

(defn copy-buffer-backed-gpu-array-to-new-lisp-data ((src gpu-array-bb))
    list
  (with-gpu-array-as-c-array (x src :access-type :read-only)
    (cepl.c-arrays::copy-c-array-to-new-lisp-data x)))

(defn copy-c-array-to-buffer-backed-gpu-array ((src c-array)
                                               (dst gpu-array-bb))
    gpu-array-bb
  (let* ((type (gpu-array-bb-element-type dst))
         (ob-dimen (c-array-dimensions src))
         (des-dimen (gpu-array-dimensions dst)))
    (if (and (eq (element-type src) type)
             (if (= 1 (length des-dimen) (length ob-dimen))
                 (<= (first ob-dimen) (first des-dimen))
                 (equal ob-dimen des-dimen)))
        (cepl.gpu-buffers::gpu-array-sub-data dst src :types-must-match t)
        (error "If the arrays are 1D then the length of the source array must
be <= length of the destination array. If the arrays have more than 1
dimension then their sizes must match exactly"))
    dst))

(defn copy-lisp-data-to-buffer-backed-gpu-array ((src (or list array))
                                                 (dst gpu-array-bb))
    gpu-array-bb
  (with-c-array-freed
      (tmp (make-c-array src
                         :dimensions (gpu-array-dimensions dst)
                         :element-type (element-type dst)))
    (copy-c-array-to-buffer-backed-gpu-array tmp dst)))


(defmethod push-g ((object list) (destination gpu-array-bb))
  (copy-lisp-data-to-buffer-backed-gpu-array object destination))
(defmethod push-g ((object c-array) (destination gpu-array-bb))
  (copy-c-array-to-buffer-backed-gpu-array object destination))

(defmethod pull1-g ((object gpu-array-bb))
  (copy-buffer-backed-gpu-array-to-new-c-array object))

(defmethod pull-g ((object gpu-array-bb))
  (copy-buffer-backed-gpu-array-to-new-lisp-data object))

(defmethod copy-g ((source list) (destination gpu-array-bb))
  (copy-lisp-data-to-buffer-backed-gpu-array source destination))
(defmethod copy-g ((source array) (destination gpu-array-bb))
  (copy-lisp-data-to-buffer-backed-gpu-array source destination))
(defmethod copy-g ((source c-array) (destination gpu-array-bb))
  (copy-c-array-to-buffer-backed-gpu-array source destination))
(defmethod copy-g ((source gpu-array-bb) (destination (eql :c-array)))
  (declare (ignore destination))
  (copy-buffer-backed-gpu-array-to-new-c-array source))
(defmethod copy-g ((source gpu-array-bb) (destination (eql :lisp)))
  (declare (ignore destination))
  (copy-buffer-backed-gpu-array-to-new-lisp-data source))

;;------------------------------------------------------------------------

;; {TODO} make a PR to add this to cl-opengl
(defmacro with-buffer-range-mapped ((p target offset length access) &body body)
  (assert (or (numberp access) (and (listp access) (every #'keywordp access))))
  (alexandria:once-only (target offset length)
    (let ((access (cffi:foreign-bitfield-value
                   '%gl::mapbufferusagemask
                   access)))
      `(let ((,p (%gl:map-buffer-range ,target ,offset ,length ,access)))
         (release-unwind-protect (progn ,@body)
           (%gl:unmap-buffer ,target))))))

(defun+ %process-with-gpu-array-range-macro-args (target access-set)
  (assert (keywordp target))
  (let* ((valid-set-elems (cffi:foreign-bitfield-symbol-list
                           '%gl::mapbufferusagemask))
         (access-set (if (and (listp access-set)
                              (eq (first access-set) 'quote))
                         (second access-set)
                         access-set))
         (access-set (uiop:ensure-list access-set)))
    (assert (loop :for access-elem :in access-set :always
                 (find access-elem valid-set-elems))
              () "The access argument must be one or more of:~{~%~s~}"
              valid-set-elems)
    access-set))

(defun+ %process-with-gpu-array-range-runtime (gpu-array start length)
  (unless (typep gpu-array 'gpu-array)
    (if (typep gpu-array 'gpu-array-t)
        (error "Unfortunately cepl doesnt not support texture backed gpu-array right now, it should, and it will...But not today. Prod me with a github issue if you need this urgently")
        (error "with-gpu-array-* does not support the type ~s"
               (type-of gpu-array))))
  (assert (= (length (gpu-array-dimensions gpu-array)) 1) ()
          "with-gpu-array-range-* macros current only support single dimensional arrays.~%Array provided: ~s" gpu-array)
  (let ((arr-len (first (gpu-array-dimensions gpu-array)))
        (byte-start (+ (gpu-array-bb-offset-in-bytes-into-buffer gpu-array)
                       (* (gpu-array-bb-element-byte-size gpu-array)
                          start)))
        (byte-len (* (gpu-array-bb-element-byte-size gpu-array)
                     length)))
    (assert (<= (- length start) arr-len))
    (list gpu-array byte-start byte-len)))

(defmacro with-gpu-array-range-as-pointer
    ((temp-name gpu-array start-index length
                &key (access-set :map-read) (target :array-buffer))
     &body body)
  (alexandria:with-gensyms (glarray-pointer array-sym gtarget
                            byte-start byte-len)
    (let ((access-set
           (%process-with-gpu-array-range-macro-args
            target access-set)))
      (labels ((tgt () (if (keywordp target) target gtarget)))
        `(dbind (,array-sym ,byte-start ,byte-len)
             (%process-with-gpu-array-range-runtime
              ,gpu-array ,start-index ,length)
           (,@(if (keywordp target)
                  '(progn)
                  `(let* ((,gtarget ,target))))
              (setf (cepl.context:gpu-buffer-bound
                     (cepl.context:cepl-context) ,(tgt))
                    (gpu-array-buffer ,array-sym))
              (with-buffer-range-mapped
                  (,glarray-pointer ,(tgt) ,byte-start ,byte-len ,access-set)
                (if (pointer-eq ,glarray-pointer (null-pointer))
                    (error "with-gpu-array-as-*: buffer mapped to null pointer~%Have you defintely got an OpenGL context?~%~s"
                           ,glarray-pointer)
                    (let ((,temp-name ,glarray-pointer))
                      ,@body)))))))))

(defmacro with-gpu-array-range-as-c-array
    ((temp-name gpu-array start-index length &key (access-set :map-read))
     &body body)
  (alexandria:with-gensyms (ggpu-array ptr start len)
    `(let ((,ggpu-array ,gpu-array)
           (,start ,start-index)
           (,len ,length))
       (with-gpu-array-range-as-pointer
           (,ptr ,ggpu-array ,start ,len :access-set ,access-set)
         (let ((,temp-name
                (make-c-array-from-pointer ,len (element-type ,ggpu-array)
                                           ,ptr)))
           ,@body)))))

;;------------------------------------------------------------------------

(defun+ reallocate-gpu-array (gpu-array)
  (assert (typep gpu-array 'gpu-array-bb) ()
          "CEPL: reallocate-gpu-array is not yet implemented for texture backed arrays")
  (reallocate-buffer (gpu-array-bb-buffer gpu-array)))

;;------------------------------------------------------------------------
