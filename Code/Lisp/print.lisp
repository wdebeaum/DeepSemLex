;;;; print.lisp - methods for printing lexicon data

(in-package :dsl)

(defmethod print-object ((c concept) (s stream))
  (write `(
      ,(type c) ,(name c)
      ,@(mapcar (lambda (r) (list (label r) (name (target r)))) ;; TODO handle OR
		(out c))
      ,@(examples c)
      ,@(definitions c)
      ;; TODO
      )))

