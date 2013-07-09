;;;; print.lisp - methods for printing lexicon data

(in-package :dsl)

(defgeneric listify (o)
  (:documentation "Turn an object into a pure nested list representation for printing."))

;; Lists are already lists, but their items need to be listified
(defmethod listify ((l list))
  (mapcar #'listify l))

;; By default, non-lists (atoms) listify to themselves. This covers things like
;; symbols, strings and numbers.
(defmethod listify ((a atom))
  a)

(defmethod listify ((c concept))
  `(
    ,(type c) ,(name c)
    ,@(mapcar (lambda (r) (list (label r) (name (target r)))) ;; TODO handle OR
	      (out c))
    ,@(mapcar (lambda (ex) (cons 'example (cdr (listify ex)))) (examples c))
    ,@(mapcar (lambda (def) (cons 'definition (cdr (listify def)))) (definitions c))
    ))

(defmethod print-object ((c concept) (s stream))
  (write (listify c) s))

(defmethod listify ((f sem-feats))
  (append (call-next-method) (features f)))

(defmethod listify ((f syn-feats))
  (append (call-next-method) (features f)))

(defmethod listify (( 
