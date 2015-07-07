(in-package :cepl)

(select 5 'thing () () ())

;; do we specify a single target and the path from that?
;; but we will want multiple targets so thats shitty
;; ...hmm
;;
;;

(select 5 (obj :some :path :to :list) (obj2 :path) (:ptr)
        :into x :type test)

(copy ((:data) (:stuff) (:more)) :count 10 :type 'some-struct)

(copy ((:data) (:stuff) (:more)) :count 10
      :type 'some-struct :into some-forgeign-ob)

;; defstruct-g
;; Ok so need to generate struct using autowrap
;; you dont get a cstruct from autowrap
;; but our accessors can use c-plus-lisp is that's ok
;; So we make the record so it is in autowrap's tables
;; Accessors are inlined functions just containing c-plus-lisp
;; paths.
