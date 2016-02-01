(defun-glsl test ((vertex :vec3) &uniform ("projection_matrix" :mat4)
                  ("modelview_matrix" :mat4)
                  &context (:vertex))
  "mat4 projection_matrix;
   mat4 modelview_matrix;

   in vec3 vertex;
   out vec3 color;

   void main(void) {
       gl_Position = projection_matrix * modelview_matrix * vec4(vertex, 1.0);
       color = vec4(1,0,0,0);
   }"
  ("color" :vec3))
