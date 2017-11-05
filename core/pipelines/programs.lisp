(in-package :cepl.pipelines)
(in-readtable :fn.reader)

;;--------------------------------------------------------------

(defn-inline use-program ((ctx cepl-context) (program-id gl-id)) gl-id
  (%with-cepl-context-slots (current-program) ctx
    (unless (= program-id current-program)
      (%gl:use-program program-id)
      (setf current-program program-id)))
  program-id)

(defn-inline force-use-program ((ctx cepl-context) (program-id gl-id)) gl-id
  (%with-cepl-context-slots (current-program) ctx
    (%gl:use-program program-id)
    (setf current-program program-id)))

;;--------------------------------------------------------------

(defun+ program-uniform-count (prog-id)
  "Returns the number of uniforms used by the shader"
  (gl:get-program prog-id :active-uniforms))

(defun+ program-uniforms (program-id)
  "Returns a list of details of the uniforms used by
   the program. Each element in the list is a list in the
   format: (uniform-name uniform-type uniform-size)"
  (loop :for i :from 0 :below (program-uniform-count program-id) :collect
     (multiple-value-bind (size type name) (gl:get-active-uniform program-id i)
       (list name type size))))

;;--------------------------------------------------------------

(defun+ program-attrib-count (program)
  "Returns the number of attributes used by the shader"
  (gl:get-program program :active-attributes))

(defun+ program-attributes (program)
  "Returns a list of details of the attributes used by
   the program. Each element in the list is a list in the
   format: (attribute-name attribute-type attribute-size)"
  (loop :for i :from 0 :below (program-attrib-count program) :collect
     (multiple-value-bind (size type name) (gl:get-active-attrib program i)
       (list name type size))))

;;--------------------------------------------------------------
