#||

- add a deftype for a buffer-id type. Use it for +unknown-id+ and structs

- CEPL context exist BEFORE gl-contexts. Very important!

- cepl.context becomes cepl.gl-context & make cepl.cepl-c..really?!
  that's gross

- remove *gl-context*

- change gl-context methods to funcs

- add vao-id type to structs in cepl-types.lisp

||#




#|| Notes

:array-buffer :array-buffer-binding     Vertex attributes
:atomic-counter-buffer :atomic-counter-buffer-binding     Atomic counter storage
:copy-read-buffer :copy-read-buffer-binding     Buffer copy source
:copy-write-buffer :copy-write-buffer-binding     Buffer copy destination
:draw-indirect-buffer :draw-indirect-buffer-binding     Indirect command arguments
:dispatch-indirect-buffer :dispatch-indirect-buffer-binding     Indirect compute dispatch commands
:element-array-buffer :element-array-buffer-binding     Vertex array indices
:pixel-pack-buffer :pixel-pack-buffer-binding     Pixel read target
:pixel-unpack-buffer :pixel-unpack-buffer-binding     Texture data source
:shader-storage-buffer :shader-storage-buffer-binding     Read-write storage for shaders
:transform-feedback-buffer :transform-feedback-buffer-binding     Transform feedback buffer
:uniform-buffer :uniform-buffer-binding
:texture-buffer     texture data buffer
:query-buffer     query result buffer

||#
