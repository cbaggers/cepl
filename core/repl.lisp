(in-package :cepl)

(defun repl (&optional (width 320) (height 240))
  (init width height "CEPL REPL" t)
  (format t "~%-----------------~%    CEPL-REPL    ~%-----------------~%"))

(defun init (&optional (width 320) (height 240) (title "CEPL") (resizable t))
  (handler-case
      (find-method #'cepl.host:init nil nil)
    (error () (error "Cepl.Host: Init could not be found. Have you loaded a host?")))
  (cepl.host:init)
  (cepl.context:make-context :width width :height height :resizable resizable
			     :title title)
  (cepl.lifecycle::change-state :interactive))

(defun init-repl-link (&key (port 4005))
  ;; handle some osx package managers
  #+darwin
  (let ((extra-package-dirs '("/opt/local/lib/" "/usr/local/")))
    (mapcar
     (lambda (raw-path)
       (let ((port-dir (cl-fad:directory-exists-p raw-path)))
         (when (and port-dir
                    (not (member port-dir cffi:*foreign-library-directories*)))
           (push port-dir cffi:*foreign-library-directories*))))
     extra-package-dirs))

  ;; start swank or slynk in a way that allows repl usage with windows&osx
  ;; window-manager thread crap
  (cond
    ((find-package :swank)
     (cepl.host:set-primary-thread-and-run
      (lambda ()
	(let (#+linux
	      (style (cepl-utils:ni-val :swank :*COMMUNICATION-STYLE*))
	      #-linux
	      (style nil))
	  (cepl-utils:ni-call :swank :create-server :style style :dont-close t :port port)))))
    ((find-package :slynk)
     (cepl.host:set-primary-thread-and-run
      (lambda ()
	(let (#+linux
	      (style (cepl-utils:ni-val :slynk :*COMMUNICATION-STYLE*))
	      #-linux
	      (style nil))
	  (cepl-utils:ni-call :slynk :create-server :style style :dont-close t :port port)))))))

(defun quit () (cepl.lifecycle::change-state :shutting-down))

(defun step-host ()
  (cepl.host::host-step))
