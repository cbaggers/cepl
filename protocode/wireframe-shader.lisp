;; Method from: http://www.imm.dtu.dk/~janba/Wireframe/

(defun-g wireframe-vert ( &uniform (win-scale :vec2) (some-transform :mat4))
  (let (;; We store the vertex id (0,1, or 2) in the w coord of the vertex
        (swizz (w (gl-vertex)))
        ;; Normal transform of vertex
        (pos (* (v! (s~ gl-vertex :xyz) 1.0)
                some-transform))
        ;; p0 is the 2D position of the current vertex
        (p0 (/ (s~ pos :xy) (w pos)))
        (p1-3d- (* some-transform p1-3d-))
        (v1 (* win-scale (- (/ (s~ p1-3d- :xy) (w p1-3d-)) p0)))        
        (p2-3d- (* some-transform p2-3d-))
        ;; Project p1 & p2 and compute v1 = p1-p0 & v2 = p2-p0
        (v2 (* win-scale (- (/ (s~ p2-3d- :xy) (w p2-3d-)) p0)))
        ;; 2D area of triangle
        (area-2d (abs (- (* (x v1) (y v2)) (* (y v1) (x v2)))))
        ;; distance from vertex to line in 2D coords
        (h (/ area-2d (length (- v1 v2)))))
    (cond ((< swizz 0.1) (setf dist (v! h 0 0)))
          ((< swizz 1.1) (setf dist (v! 0 h 0)))
          (t (setf dist (v! 0 0 h))))
    (values pos
            ;;negate perspective correction
            (* dist (w pos)))))


(defun-g wireframe-frag ((dist :vec3) &uniform (wire-color :vec3)
                         (fill-color :vec3))
  (let* (;; Undo perspective correction
         (dist-vec (* dist (w gl-frag-coord)))
         ;; shortest distance to the edge
         (d (min (x dist-vec) (y dist-vec) (z dist-vec)))
         (i (exp2 (* d d -2)))) ;; line intensity    
    (+ (* i wire-color) (* (- 1.0 i) fill-color))))


(defpipeline wireframe (g-> #'wireframe-vert #'wireframe-frag))
