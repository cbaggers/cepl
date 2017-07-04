;; Syntax for instanced arrays

(make-buffer-stream (list a b c)
                    :index-array i
                    :instance '(nil nil 1))

(make-buffer-stream (list a b c)
                    :index-array i
                    :instance '((2 1)))

(make-buffer-stream (list a b c)
                    :index-array i
                    :instance '((2 . 1)))

(make-buffer-stream (list a b (per-instance c 1))
                    :index-array i)

(make-buffer-stream (list a b (per c 1))
                    :index-array i)

(make-buffer-stream (list a b (cons c 1))
                    :index-array i)

(make-buffer-stream (list a b (list c 1))
                    :index-array i)
