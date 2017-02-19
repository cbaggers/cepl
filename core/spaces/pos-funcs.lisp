(in-package :cepl.space)

(defun sv! (vec &optional (space *default-pos-space*))
  (case= (length vec)
    ;; (3 (%%-svec3! :space space :point vec))
    (3 (error "CEPL: Spatial vec3 is not yet supported. Sorry for the inconvenience"))
    (4 (%%-svec4! :space space :point vec))))

(defun to-space (destination-space pos)
  (typecase pos
    ;; (svec3 (m4:*v (get-transform (pos-space pos) destination-space)
    ;;               (svec3-point pos)))
    (svec4 (m4:*v (get-transform (pos-space pos) destination-space)
                  (svec4-point pos)))))

(defun re-space (new-space pos)
  "makes a new point in the same location as the first but relative to the
   provided new space"
  (typecase pos
    ;; (svec3 (sv! (m4:*v (get-transform (pos-space pos) new-space)
    ;;                    (svec3-point pos))
    ;;             new-space))
    (svec4 (sv! (m4:*v (get-transform (pos-space pos) new-space)
                       (svec4-point pos))
                new-space))))
