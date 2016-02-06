(ql:quickload "(#| TMPL_VAR name |#)")

(funcall
 (symbol-function
  (find-symbol "RUN-SESSION"
	       (find-package (string-upcase "(#| TMPL_VAR name |#)")))))
