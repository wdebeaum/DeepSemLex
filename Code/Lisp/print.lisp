;;;; print.lisp - methods for printing lexicon data

;;;; TODO
;;;; - track lexical context vars and e.g. avoid reprinting the same provenance over and over
;;;; - word, morph, sense
;;;; - combine relations with the same name
;;;; - standardize order of slots/relations
;;;; - separate implicit inheritance from explicit

(in-package :dsl)

;; We print concepts by listifying them and writing the list.
(defmethod print-object ((c concept) s)
  (let ((*package* (find-package :ld)))
    (write (listify c) :stream s)))

(defmethod listify (x)
  (cond
    ;; Lists are already lists, but their items need to be listified.
    ((listp x)
      (mapcar #'listify x))
    ;; Convert symbols in DSL or CL packages to LD package.
    ((and (symbolp x)
          (member (symbol-package x)
	          (mapcar #'find-package '(dsl common-lisp))))
      (intern (symbol-name x)))
    ;; By default, non-lists (atoms) listify to themselves. This covers things
    ;; like explicitly packaged symbols, strings and numbers.
    (t x)
    ))

(defun listify-slots (o &optional slot-names)
    (declare (type standard-object o))
  "Get an alist corresponding to the slots and listified values of o."
  (mapcan (lambda (slot-name)
	    (when (and (slot-boundp o slot-name) (slot-value o slot-name))
	      (list (list (intern (symbol-name slot-name))
			  (let ((*print-level* 0))
			    (listify (slot-value o slot-name)))
			  ))))
          (if slot-names
	    slot-names
	    (class-slot-names (type-of o))
	    )
	  ))

(defun listify-slot-values (o &optional slot-names)
  "Get a list of listified values of bound, non-nil slots of o. This is useful
   as a replacement for listify-slots when the slot names are the same as the
   name of the class they hold, and thus the car of the list."
  (mapcan (lambda (slot-name)
            (when (and (slot-boundp o slot-name) (slot-value o slot-name))
	      (list (listify (slot-value o slot-name)))))
          (if slot-names
	    slot-names
	    (class-slot-names (type-of o))
	    )
	  ))

;; By default, objects listify to their type followed by an alist of their
;; slots.
(defmethod listify ((o standard-object))
  (cons (intern (symbol-name (type-of o))) (listify-slots o)))

(defmethods print-object ((x (or provenance input-text relation word morph)) s)
  (write (listify x) :stream s))

;; General concept listification. When *print-level* is 0, this just gets the
;; name of the concept, otherwise it gets the name followed by slots common to
;; all concepts. Subclasses of concept override this by appending to
;; (call-next-method) if it's a list.
(defmethod listify ((c concept))
  (if (and *print-level* (= 0 *print-level*) (not (anonymous-concept-p c)))
    (name c)
    `(
      ,(intern (symbol-name (type-of c)))
      ,@(unless (anonymous-concept-p c) (list (name c)))
      ,@(when (aliases c) (list (list 'ld::aliases (aliases c))))
      ,@(mapcar #'listify (provenance c))
      ;; TODO collapse relations with the same name
      ,@(mapcar (lambda (r)
                  (if (and (eq :inherit (label r))
		           (or (typep (target r) '(disjunction concept))
			       (anonymous-concept-p (target r))))
		           ; FIXME not sure this is constrained enough
		    (listify (target r))
		    (list (intern (symbol-name (label r)))
			  (let ((*print-level* 0)) (listify (target r))))
		    ))
		(out c))
      ,@(mapcar (lambda (ex)
                  (cons 'ld::example (cdr (listify ex))))
                (examples c))
      ,@(mapcar (lambda (def)
                  (cons 'ld::definition (cdr (listify def))))
                (definitions c))
      )))

(defmethod listify ((m role-restr-map))
  `(,@(listify (if (= 1 (length (roles m))) (roles m) (list (roles m))))
    ,(listify (restriction m))
    ,@(when (optional m) 'ld::optional)
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
      (append parent-list (listify (features f)))
      parent-list)))

(defmethod listify ((s semantics))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list
              (listify-slot-values s '(sem-frame sem-feats entailments)))
      parent-list)))

(defmethod listify ((m syn-sem-map))
  `(,(intern (symbol-name (syn-arg m)))
    ,(if (head-word m)
      (util::convert-to-package (list (syn-cat m) (head-word m)))
      (intern (symbol-name (syn-cat m)))
      )
    ,@(when (sem-role m) (list (intern (symbol-name (sem-role m)))))
    ,@(when (optional m) '(ld::optional))
    ))

(defmethod listify ((ss syn-sem))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (mapcar #'listify (maps ss)))
      parent-list)))

(defmethod listify ((f syn-feats))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (listify (features f)))
      parent-list)))

(defmethod listify ((s syntax))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (listify-slot-values s '(syn-sem syn-feats)))
      parent-list)))

(defmethod listify ((w word))
  (with-slots (first-word remaining-words particle) w
    (util::convert-to-package 
	(cond
	  (particle
	    `(word (,first-word ,@remaining-words (,particle))))
	  (remaining-words
	    `(word (,first-word ,@remaining-words)))
	  (t
	    `(word ,first-word))
	  ))))

(defmethod listify ((s sense))
  (let ((parent-list (call-next-method)))
    (if (listp parent-list)
      (append parent-list (listify-slot-values s '(morph syntax semantics)))
      parent-list)))

