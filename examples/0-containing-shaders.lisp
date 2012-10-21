;; This is simply to get a colored triangle up on the screen

(defvar vert "#version 330
   layout (location = 0) in vec4 position;
   layout (location = 1) in vec4 color;
   smooth out vec4 theColor;

   void main()
   {
      gl_Position = position;
      theColor = color;
   }")

(defvar frag "#version 330
   smooth in vec4 theColor;
   out vec4 outputColor;

  void main()
  {
      float lerpValue = gl_FragCoord.y / 500.0f;
      outputColor = mix(theColor,
                        vec4(0.2f, 0.2f, 0.2f, 1.0f),
		        lerpValue);
  }")

(cgl:defglstruct vert-data
  (position :type :float :length 4)
  (colour :type :float :length 4))

(defun draw (program streams)
  (gl:clear :color-buffer-bit)
  (cgl:draw-streams program streams)
  (gl:flush)
  (sdl:update-display))

(defun run-demo ()
  (cgl:clear-color 0.0 0.0 0.0 0.0)
  (gl:viewport 0 0 640 480)
  (let* ((program (cgl:make-program (cgl:make-shader vert :vertex-shader) 
				    (cgl:make-shader frag :fragment-shader)))
         (data '((#( 0.0    0.5 0.0 1.0) #( 1.0 0.0 0.0 1.0))
                 (#( 0.5 -0.366 0.0 1.0) #( 0.0 1.0 0.0 1.0))
                 (#(-0.5 -0.366 0.0 1.0) #( 0.0 0.0 1.0 1.0))))
         (streams `(,(cgl:make-gpu-stream-from-gpu-arrays
                      :length 3
                      :gpu-arrays (cgl:make-gpu-array
				   data :element-type 'vert-data)))))
    (sdl:with-events ()
      (:quit-event () t)
      (:VIDEO-RESIZE-EVENT (:w width :h height) (gl:viewport 0 0 width height))
      (:idle () (cepl-utils:update-swank)
                (base-macros:continuable (draw program streams))))))
