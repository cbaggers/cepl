;; An extension to staple that allows rendering of markdown in the doc-strings

(defmethod staple:render-docstring
    (string (system (eql (asdf:find-system :cepl))))
  (typecase string
    (string (staple:render-docstring-markdown string))
    (null (plump:parse "<i>No docstring provided.</i>"))))
