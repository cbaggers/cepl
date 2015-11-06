(in-package :spaces)

(defstruct (space-event (:include cpl-event))
  (flush nil
	 :type boolean)
  (matrix-4 (m4:identity-matrix4)
	    :type (simple-array single-float (16))))

(defstruct (space-event-node (:include cepl-event-node))
  (transform (m4:identity-matrix4)
	     :type (simple-array single-float (16)))
  (has-propagated nil :type boolean))

(defun get-transform ()
  )

(deftransform view-space->clip-space
    :from view-space
    :to clip-space
    (:cpu :forward-transform nil
	  :backward-transform nil)
    (:vertex :forward-transform '???
	     :backward-transform nil)
    (:fragment :forward-transform nil
	       :backward-transform nil))

(deftransform clip-space->screen-space
    :from clip-space
    :to screen-space
    (:cpu :forward-transform nil
	  :backward-transform nil)
    (:fragment :forward-transform :+implicit+
	       :backward-transform nil))

(transform clip-space normalize-device-space (v! 1 2 3 4))

(defun transform (from to point)
  )

(g->
     *model-and-view-transform*
     vertex-shading
     *projection*
     *clipping*
     *screen-mapping*
     *triangle-setup* ;; fixed in hardware
     *triangle-traversal* ;; fixed in hardware
     fragment-shading
     *merging)

;; For reference
;;
;; - maybe remove '(tags uid name)
;; - maybe merge filter & body
;;
;; (defstruct (cepl-event-node (:constructor %make-cepl-event-node)
;; 			    (:conc-name event-node-))
;;   (uid (gensym "EVENT-NODE-")
;;        :type symbol)
;;   (name +default-node-name+
;;         :type symbol)
;;   (tags nil
;;         :type list
;;         :read-only t)
;;   (subscribers (cons nil nil) ;; weak refs to consumers
;;                :type list
;;                :read-only t)
;;   (subscriptions (cons nil nil) ;; strong refs to nodes
;;                  :type list
;;                  :read-only t)
;;   (filter #'event-no-filter
;;           :type function
;;           :read-only t)
;;   (body #'event-no-body
;; 	:type function
;; 	:read-only t))

;; example event-node
;;
;; (defvar |window|
;;   (make-event-node
;;    :name 'cepl-window
;;    :tags '(:window)
;;    :filter #'win-p
;;    :subscribe-to backend-events))
