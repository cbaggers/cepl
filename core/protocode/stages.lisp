(defun cpu-side ()
  (p-> #'specify-vertices
       #'render-vertices))

(defpipeline full-gl-pipeline
    (g-> ;; vertex-processing
         vertex-shader                 ;; user defined
	 #'limited-primitive-assembly
	 tessellation-shader           ;; user defined
	 geometry-shader               ;; user defined
	 ;; vertex-post-processing
	 #'transform-feedback
	 #'clip-primitives
	 #'perspective-divide
	 #'viewport-transform
	 #'primitive-assembly
	 #'cull-faces
	 ;; rasterize
	 #'rasterize
	 ;; fragment processing
	 #'fragment-shader             ;; user defined
	 ;; per sample operations
	 ;; culling tests; if a test is active and the fragment fails the test,
	 ;; the underlying pixels/samples are not updated (usually)
	 #'pixel-ownership-test
	 #'scissor-test
	 #'stencil-test
	 #'depth-test
	 ;; blending
	 #'color-blending
	 ;; output
	 #'write-to-framebuffer
))

(defun-g pixel-ownership-test ()
  "Fails if the fragment's pixel is not \"owned\" by OpenGL (if
   another window is overlapping with the GL window). Always passes
   when using a Framebuffer Object. Failure means that the pixel
   contains undefined values.")

(defun-g scissor-test ()
  "When enabled, the test fails if the fragment's pixel lies outside
   of a specified rectangle of the screen.")

(defun-g stencil-test ()
  "When enabled, the test fails if the stencil value provided by the
   test does not compare as the user specifies against the stencil
   value from the underlying sample in the stencil buffer. Note that
   the stencil value in the framebuffer can still be modified even if
   the stencil test fails (and even if the depth test fails).")

(defun-g depth-test ()
  "When enabled, the test fails if the fragment's depth does not
   compare as the user specifies against the depth value from the
   underlying sample in the depth buffer.")

(defun-g color-blending ()
  "For each fragment color value, there is a specific blending
   operation between it and the color already in the framebuffer at
   that location. Logical Operations may also take place in lieu of
   blending, which perform bitwise operations between the fragment
   colors and framebuffer colors.")

(defun-g write-to-framebuffer ()
  "The fragment data is written to the framebuffer. Masking
  operations allow the user to prevent writes to certain
  values. Color, depth, and stencil writes can be masked on and off;
  individual color channels can be masked as well.")


(defun-g limited-primitive-assembly ()
  "If tessellation or geometry shaders are active, then a limited form of
   primitive assembly is executed before these Vertex Processing
   stages. This is used to feed those particular shader stages with
   individual primitives, rather than a sequence of vertices.")

(defun-g transform-feedback ()
  "The outputs of the geometry shader or primitive assembly are
  written to a series of buffer objects that have been setup for this
  purpose. This is called transform feedback mode; it allows the user
  to do transform data via vertex and geometry shaders, then hold on
  to that data for use later.

  The data output into the transform feedback buffer is the data from
  each primitive emitted by this step."
  nil)

(defun-g clip-primitives ()
  "The primitives are then clipped. Clipping means that primitives
  that lie on the boundary between the inside of the viewing volume
  and the outside are split into several primitives, such that the
  entire primitive lies in the volume. Also, the last Vertex
  Processing shader stage can specify user-defined clipping
  operations, on a per-vertex basis."
  nil)

(defun-implicit-g perspective-divide (vertex)
  "The clip-space positions returned from the clipping stage are
  transformed into normalized device coordinates (NDC) via this
  equation:

  (let ((normalized-device-coords        ;; (v! ndc-x ndc-y ndc-z)
	 (v! (/ x w) (/ y w) (/ z w))))
    ...)"
  (transform-to +ndc-space+ (coord vertex)))

(defvar +ndc-space+ (define-implicit-space))
(defvar +window-space+ (define-implicit-space))

(defun-g viewport-transform ()
  "The viewport transform defines the transformation of vertex
   positions from NDC space to window space. These are the coordinates
   that are rasterized to the output image.

   The viewport is defined by a number of viewport parameters. These
   parameters are set by these functions:

   (gl:viewport x y width height)
   (gl:depth-range nearval farval) ;; and it's siblings

   Given the viewport parameters, we compute the window-space coordinates
   via these equations:

   (let ((window-space-coords                         ;; (v! w-x w-y w-z)
	  (v! (+ (* (/ width 2) ndc-x) x (/ width 2))
	      (+ (* (/ height 2) ndc-y) y (/ height 2))
	      (+ (* (/ (- far near) 2) ndc-z) (/ (+ far near) 2)))))
     ...)

   Where x​, y​, width​, height​, nearVal​, and farVal​ are the viewport parameters."
  (transform-to +window-space+ (coord vertex)))

(defun-g primitive-assembly
    "Primitive assembly is the process of collecting a run of vertex
    data output from the prior stages and composing it into a sequence
    of primitives. The type of primitive the user rendered with
    determines how this process works.

    The output of this process is an ordered sequence of simple primitives
    (lines, points, or triangles). If the input is a triangle strip
    primitive containing 12 vertices, for example, the output of this
    process will be 10 triangles.

    The rendering pipeline can also be aborted at this stage. This allows
    the use of Transform Feedback operations, without having to actually
    render something."
  nil)

(defun-g cull-faces ()
  "Triangle primitives can be culled (ie: discarded without rendering)
   based on the triangle's facing in window space. This allows you to
   avoid rendering triangles facing away from the viewer. For closed
   surfaces, such triangles would naturally be covered up by triangles
   facing the user, so there is never any need to render them. Face
   culling is a way to avoid rendering such primitives."
  nil)

(defun-g rasterize ()
  "Primitives that reach this stage are then rasterized in the order
   in which they were given. The result of rasterizing a primitive is a
   sequence of Fragments.

   A fragment is a set of state that is used to compute the final data
   for a pixel (or sample if multisampling is enabled) in the output
   framebuffer. The state for a fragment includes its position in
   screen-space, the sample coverage if multisampling is enabled, and a
   list of arbitrary data that was output from the previous vertex or
   geometry shader.

   This last set of data is computed by interpolating between the data
   values in the vertices for the fragment. The style of interpolation is
   defined by the shader that outputed those values.")

;;----------------------------------------------------------------------
;; CPU
(defun render-vertices ()
  "Vertex Rendering is the process of taking vertex data specified in
  arrays and rendering one or more Primitives with this vertex data."
  nil)

(defun specify-vertices ()
  "Vertex Specification Main article: Vertex Specification

   The process of vertex specification is where the application sets up
   an ordered list of vertices to send to the pipeline. These vertices
   define the boundaries of a primitive.

   Primitives are basic drawing shapes, like triangles, lines, and
   points. Exactly how the list of vertices is interpreted as primitives
   is handled via a later stage.

   This part of the pipeline deals with a number of objects like Vertex
   Array Objects and Vertex Buffer Objects. Vertex Array Objects define
   what data each vertex has, while Vertex Buffer Objects store the
   actual vertex data itself.

   A vertex's data is a series of attributes. Each attribute is a small
   set of data that the next stage will do computations on. While a set
   of attributes do specify a vertex, there is nothing that says that
   part of a vertex's attribute set needs to be a position or
   normal. Attribute data is entirely arbitrary; the only meaning
   assigned to any of it happens in the vertex processing stage."
  nil)
