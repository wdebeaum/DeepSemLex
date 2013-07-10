;;;; print.lisp - methods for printing lexicon data

(in-package :dsl)

(defgeneric listify (o)
  (:documentation "Turn an object into a pure nested list representation for printing."))

;; We print concepts by listifying them and writing the list.
(defmethod print-object ((c concept) s)
  (write (listify c) :stream s))

;; Unfortunately, we can't specialize on list and atom because they're not
;; classes.
(defmethod listify (x)
  (if (listp x)
    ;; Lists are already lists, but their items need to be listified.
    (mapcar #'listify x)
    ;; By default, non-lists (atoms) listify to themselves. This covers things
    ;; like symbols, strings and numbers.
    x))

(defun listify-slots (o &optional slot-names)
    (declare (type standard-object o))
  "Get an alist corresponding to the slots and listified values of o."
  (mapcan (lambda (slot-name)
	    (when (and (slot-boundp o slot-name) (slot-value o slot-name))
	      (list (list slot-name
			  (let ((*print-level* 0))
			    (listify (slot-value o slot-name)))
			  ))))
          (if slot-names
	    slot-names
	    (class-slot-names (type-of o))
	    )
	  ))

;; By default, objects listify to their type followed by an alist of their
;; slots. This covers things like provenance, input-texts, and relations.
(defmethod listify ((o standard-object))
  (cons (type-of o) (listify-slots o)))

(defmethod print-object ((p provenance) s)
  (write (listify p) :stream s))

(defmethod print-object ((it input-text) s)
  (write (listify it) :stream s))

(defmethod print-object ((r relation) s)
  (write (listify r) :stream s))

;; General concept listification. When *print-level* is 0, this just gets the
;; name of the concept, otherwise it gets the name followed by slots common to
;; all concepts. Subclasses of concept override this by appending to
;; (call-next-method) if it's a list.
(defmethod listify ((c concept))
  (if (and *print-level* (= 0 *print-level*))
    (name c)
    `(
      ,(type-of c) ,@(unless (anonymous-concept-p c) (list (name c)))
      ,@(when (aliases c) (list (list 'aliases (aliases c))))
      ,@(mapcar #'listify (provenance c))
      ;; TODO collapse relations with the same name
      ,@(mapcar (lambda (r)
                  (if (and (eq :inherit (label r))
		           (anonymous-concept-p (target r)))
		           ; FIXME not sure this is constrained enough
		    (listify (target r))
		    (list (intern (symbol-name (label r)))
			  (let ((*print-level* 0)) (listify (target r))))
		    ))
		(out c))
      ,@(mapcar (lambda (ex)
                  (cons 'example (cdr (listify ex))))
                (examples c))
      ,@(mapcar (lambda (def)
                  (cons 'definition (cdr (listify def))))
                (definitions c))
      )))

(defmethod listify ((m role-restr-map))
  `(,@(if (= 1 (length (roles m))) (roles m) (list (roles m)))
    ,(listify (restriction m))
    ,@(when (optional m) 'optional)
    ))

(defmethod listify ((f sem-frame))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (mapcar #'listify (maps f)))
      parent-list)))

(defmethod listify ((e entailments))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (terms e))
      parent-list)))

(defmethod listify ((f sem-feats))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (features f))
      parent-list)))

(defmethod listify ((s semantics))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list
              (mapcan (lambda (slot-name)
	                (when (slot-boundp s slot-name)
			  (list (listify (slot-value s slot-name)))))
	              '(sem-frame sem-feats entailments)
		      ))
      parent-list)))

(defmethod listify ((m syn-sem-map))
  `(,(syn-arg m)
    ,(if (head-word m)
      (list (syn-cat m) (head-word m))
      (syn-cat m)
      )
    ,@(when (sem-role m) (list (sem-role m)))
    ,@(when (optional m) '(optional))
    ))

(defmethod listify ((ss syn-sem))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (mapcar #'listify (maps ss)))
      parent-list)))

(defmethod listify ((f syn-feats))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (features f))
      parent-list)))

(defmethod listify ((s syntax))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list
              (mapcan (lambda (slot-name)
	                (when (slot-boundp s slot-name)
			  (list (listify (slot-value s slot-name)))))
	              '(syn-sem syn-feats)
		      ))
      parent-list)))

