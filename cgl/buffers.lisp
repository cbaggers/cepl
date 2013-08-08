(in-package :cgl)
;;;--------------------------------------------------------------
;;; BUFFERS ;;;
;;;---------;;;

(defstruct glbuffer
  "This is our opengl buffer object. Along with the opengl
   buffer name (buffer-id) we also store the layout of the data
   within the buffer.
   This layout is as follows:
   `((data-type data-index-length offset-in-bytes-into-buffer))
   for example:
   `((:float 12 0) ('vert-data 140 12))"
  (buffer-id (car (gl:gen-buffers 1)))
  (format nil))

;; [TODO] Implement buffer freeing properly
(let ((buffer-pool nil))
  (defun add-buffer-to-pool (buffer)
    (setf buffer-pool (cons buffer buffer-pool))
    buffer)
  (defun free-all-buffers-in-pool ()
    (mapcar #'(lambda (x) (declare (ignore x))
                      (print "freeing a buffer")) 
            buffer-pool)))

;; [TODO] This needs a rework given how gl targets operate
(let ((buffer-id-cache nil)
      (buffer-target-cache nil))
  (defun bind-buffer (buffer buffer-target)
    "Binds the specified opengl buffer to the target"
    (let ((id (glbuffer-buffer-id buffer)))
      (unless (and (eq id buffer-id-cache) 
                   (eq buffer-target buffer-target-cache))
        (cl-opengl-bindings:bind-buffer buffer-target id)
        (setf buffer-target-cache id)
        (setf buffer-target-cache buffer-target))))
  (defun force-bind-buffer (buffer buffer-target)
    "Binds the specified opengl buffer to the target"
    (let ((id (glbuffer-buffer-id buffer)))
      (cl-opengl-bindings:bind-buffer buffer-target id)
      (setf buffer-id-cache id)
      (setf buffer-target-cache buffer-target)))
  (defun unbind-buffer ()
    (cl-opengl-bindings:bind-buffer :array-buffer 0)
    (setf buffer-id-cache 0)
    (setf buffer-target-cache :array-buffer)))

(defun gen-buffer (&key initial-contents 
                     (buffer-target :array-buffer) 
                     (usage :static-draw))
  (declare (symbol buffer-target usage))
  "Creates a new opengl buffer object. 
   Optionally you can provide a c-array as the :initial-contents
   to have the buffer populated with the contents of the array"
  (let ((new-buffer (make-glbuffer)))
    (if initial-contents
        (buffer-data new-buffer initial-contents buffer-target
                     usage)
        new-buffer)))

(defun buffer-data-raw (data-pointer data-type data-byte-size
                        buffer buffer-target usage &optional (byte-offset 0))
  "This function populates an opengl buffer with the contents 
   of the array. You also pass in the buffer type and the 
   draw type this buffer is to be used for.
   
   The function returns a buffer object with its format slot
   populated with the details of the data stored within the buffer"
  (bind-buffer buffer buffer-target)
  (%gl:buffer-data buffer-target data-byte-size
                   (cffi:inc-pointer data-pointer byte-offset)
                   usage)
  (setf (glbuffer-format buffer) `((,data-type ,data-byte-size 0)))
  buffer)

(defun buffer-data (buffer c-array buffer-target usage
                    &key (offset 0) (size (c-array-byte-size c-array)))
  "This function populates an opengl buffer with the contents 
   of the array. You also pass in the buffer type and the 
   draw type this buffer is to be used for.
   
   The function returns a buffer object with its format slot
   populated with the details of the data stored within the buffer"
  (let ((data-type (element-type c-array)))
    (buffer-data-raw (pointer c-array) data-type size buffer buffer-target usage
                     (* offset (cffi:foreign-type-size data-type)))))

;; [TODO] doesnt check for overflow off end of buffer
(defun buffer-sub-data (buffer c-array byte-offset buffer-target
                        &key (safe t))  
  "This function replaces a subsection of the data in the 
   specified buffer with the data in the c-array.
   The byte offset specifies where you wish to start overwriting 
   data from. 
   When the :safe option is t, the function checks to see if the 
   data you are about to write into the buffer will cross the 
   boundaries between data already in the buffer and will emit 
   an error if you are."
  (let ((byte-size (c-array-byte-size c-array)))
    (when (and safe (loop for format in (glbuffer-format buffer)
                       when (and (< byte-offset (third format))
                                 (> (+ byte-offset byte-size)
                                    (third format)))
                       return t))
      (error "The data you are trying to sub into the buffer crosses the boundaries specified in the buffer's format. If you want to do this anyway you should set :safe to nil, though it is not advised as your buffer format would be invalid"))
    (bind-buffer buffer buffer-target)
    (%gl:buffer-sub-data buffer-target
                         byte-offset
                         byte-size
                         (pointer c-array)))
  buffer)

(defun multi-buffer-data (buffer c-arrays buffer-target usage)
  "This beast will take a list of c-arrays and auto-magically
   push them into a buffer taking care of both interleaving 
   and sequencial data and handling all the offsets."
  (let* ((c-array-byte-sizes (loop for c-array in c-arrays
                              collect 
                                (c-array-byte-size c-array)))
         (total-size (apply #'+ c-array-byte-sizes)))
    (bind-buffer buffer buffer-target)
    (buffer-data buffer (first c-arrays) buffer-target usage
                 :size total-size)
    (setf (glbuffer-format buffer) 
          (loop :for c-array :in c-arrays
             :for size :in c-array-byte-sizes
             :with offset = 0
             :collect (list (element-type c-array) size offset)
             :do (buffer-sub-data buffer c-array offset
                                  buffer-target :safe nil)
             (setf offset (+ offset size)))))
  buffer)

(defun buffer-reserve-block-raw (buffer size-in-bytes buffer-target 
                                 usage)
  "This function creates an empty block of data in the opengl buffer.
   It will remove ALL data currently in the buffer. It also will not
   update the format of the buffer so you must be sure to handle this
   yourself. It is much safer to use this as an assistant function to
   one which takes care of these issues"
  (bind-buffer buffer buffer-target)
  (%gl:buffer-data buffer-target size-in-bytes
                   (cffi:null-pointer) usage)
  buffer)

;; [TODO] Handle alignment?
(defun buffer-reserve-block (buffer type dimensions buffer-target usage)
  "This function creates an empty block of data in the opengl buffer
   equal in size to (* length size-in-bytes-of-type).
   It will remove ALL data currently in the buffer"
  (bind-buffer buffer buffer-target)
  (let* ((dimensions (if (listp dimensions) dimensions (list dimensions)))
         (byte-size (gl-calc-byte-size type dimensions)))
    (buffer-reserve-block-raw buffer
                              byte-size
                              buffer-target
                              usage)
    (setf (glbuffer-format buffer) `((,type ,byte-size ,0))))
  buffer)

(defun buffer-reserve-blocks (buffer types-and-dimensions
                              buffer-target usage)
  "This function creates an empty block of data in the opengl buffer
   equal in size to the sum of all of the 
   (* length size-in-bytes-of-type) in types-and-lengths.
   types-and-lengths should be of the format:
   `((type length) (type length) ...etc)
   It will remove ALL data currently in the buffer"
  (let ((total-size-in-bytes 0))
    (setf (glbuffer-format buffer) 
          (loop :for (type dimensions) :in types-and-dimensions :collect
             (progn (let ((size-in-bytes (gl-calc-byte-size type dimensions)))
                      (incf total-size-in-bytes size-in-bytes)
                      `(,type ,size-in-bytes ,total-size-in-bytes)))))
    (buffer-reserve-block-raw buffer total-size-in-bytes buffer-target usage))
  buffer)
