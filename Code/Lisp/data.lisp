;;;; data.lisp - stuff to use for reading lexicon data; mostly macros

;; TODO convert symbol packages where appropriate

(in-package :lexicon-data)

;; lexical context
(defvar *concept-stack* nil)
(defun current-concept () (car *concept-stack*))
(defvar *current-provenance* nil)
(defvar *current-word* nil)
(defvar *current-morph* nil)

(defmacro operator-cond ((op-var form-var body-forms) &body cases)
  "For each of the body-forms, if the operator satisfies one of the cases'
   tests, use the body of that case to convert body-form to an expression to
   evaluate. Otherwise keep the body-form unchanged."
  (mapcar
      (lambda (body-form)
	(loop with operator = (car body-form)
	      for cs in cases
	      when (eval `(let ((,op-var ',operator)) ,(car cs)))
		return (eval `(let ((,op-var ',operator) (,form-var ',body-form)) ,(cdr cs)))
	      finally return `',body-form
	      ))
      (eval body-forms)))

(defun non-concept-class (cls body)
  "Return code to be evaluated to instantiate a non-concept class cls with the
   given body forms. Forms starting with a slot name set the slot to the second
   value in the form, while others are left to be evaluated normally (with
   *current-<cls>* set to the new instance)."
  (let ((slots (class-slot-names cls))
	(current-var (intern (concatenate 'string "*CURRENT-" (symbol-name cls) "*"))))
    `(progn
      (setf ,current-var (make-instance ',cls))
      (when (and (current-concept) (slot-exists-p (current-concept) ',cls))
	(setf (slot-value (current-concept) ',cls) ,current-var))
      ,@(operator-cond (operator form body)
	((member operator slots)
	  `(setf (slot-value ,current-var ',operator) ',(second form)))
	)
      )))

(defun optionally-named-concept-subtype (concept-type name-and-body)
  "Return code to be evaluated to instantiate or add to a concept class with
   the given body forms (the first of which may be a name). This handles
   binding the lexical context variables, instantiating the concept if
   necessary, and adding inheritance relations implicit in the lexical
   nesting. Callers should handle transforming any body forms that aren't meant
   to be evaluated as-is before calling this function."
  (let (name (body name-and-body))
    (when (symbolp (car body))
      (setf name (pop body)))
    `(let* (
	    ;; bind the lexical context variables so they get reset when we
	    ;; leave this let
	    (*concept-stack*		*concept-stack*)
            (*current-provenance*	*current-provenance*)
            (*current-word*		*current-word*)
	    (*current-morph*		*current-morph*)

            (anonymous ,(null name))
	    (outer-type (type-of (current-concept)))
	    (inner-part-of-outer (subtypep outer-type concept-type))
	    (outer-part-of-inner (subtypep concept-type outer-type))
	    )
      (cond
	((not anonymous)
	  (push (dsl::get-or-make-concept ',name ',concept-type)
		*concept-stack*)
	  (when inner-part-of-outer
	    (add-relation 
		(second *concept-stack*) :inherit (current-concept)
		*current-provenance*
		))
	  )
	((null *concept-stack*)
	  (push (make-instance ',concept-type) *concept-stack*)
	  )
	(outer-part-of-inner
	  (push (make-instance ',concept-type) *concept-stack*)
	  (add-relation
	      (current-concept) :inherit (second *concept-stack*)
	      *current-provenance*
	      )
	  )
	(inner-part-of-outer
	  (first *concept-stack*))
	(t
	  (error "incompatible nested concept types: ~s inside ~s" `,concept-type outer-type))
	)
      (unless (or (null *current-provenance*)
                  (member *current-provenance*
		          (provenance (current-concept))
			  :test #'eq))
        (push *current-provenance* (provenance (current-concept))))
      ,@body
      )
    ))

(defmacro provenance (&body body)
  (non-concept-class 'dsl::provenance body))

(defmacro concept (&body body)
  (optionally-named-concept-subtype 'dsl::concept body))

(defmacro sem-frame (&body body)
  (optionally-named-concept-subtype 'dsl::sem-frame
      (operator-cond (operator form body)
        ((typep operator '(or sem-role (list-of sem-role)))
	  (destructuring-bind (roles restriction &optional optional) form
	    `(push 
		(make-instance 'dsl::role-restr-map
		    :roles ',(if (listp roles) roles (list roles))
		    :restriction ,restriction ;; TODO autovivify symbols as concepts?
		    :optional ,(not (null optional))
		    )
		(maps (current-concept))
		)))
	)
      ))

(defmacro entailments (&body body)
  (optionally-named-concept-subtype 'dsl::sem-frame body)) ; TODO

(defmacro semantics (&body body)
  (optionally-named-concept-subtype 'dsl::semantics body))

(defmacro syn-sem (&body body)
  (optionally-named-concept-subtype 'dsl::syn-sem
      (operator-cond (operator form body)
        ((typep operator 'dsl::syn-arg)
	  (destructuring-bind (syn-arg syn-cat &optional sem-role optional) form
	    `(push
		(make-instance 'dsl::syn-sem-map
		    :syn-arg ',syn-arg
		    :syn-cat ',(if (listp syn-cat) (car syn-cat) syn-cat)
		    :head-word ',(when (listp syn-cat) (second syn-cat))
		    :sem-role ',sem-role
		    :optional ,(not (null optional))
		    )
		(maps (current-concept))
		)))
	)))

(defmacro syntax (&body body)
  (optionally-named-concept-subtype 'dsl::syntax body))

(defun make-word-from-spec (spec)
  "Make an instance of the dsl::word class from a specification like the
  following:
    singleword
    still-a-single-word
    (multiple contiguous words)
    (word with (particle))
  "
  (setf spec (convert-to-package spec :w))
  (cond
    ((symbolp spec)
      (make-instance 'dsl::word
          :first-word spec))
    ((not (listp spec))
      (error "bogus word spec; expected symbol or list, but got: ~s" spec))
    ((every #'symbolp spec)
      (make-instance 'dsl::word
          :first-word (car spec)
	  :remaining-words (cdr spec)
	  ))
    ((and (every #'symbolp (butlast spec))
          (typep (car (last spec)) '(cons symbol null)))
      (make-instance 'dsl::word
          :first-word (car spec)
	  :remaining-words (butlast (cdr spec))
	  :particle (caar (last spec))
	  ))
    (t
      (error "bogus word spec; expected list of symbols with possible final list of one particle symbol, but got: ~s"))
    ))

(defmacro word (word-spec &body body)
  `(let ((*current-word* (make-word-from-spec ',word-spec)))
    ,@body))

(defmacro morph (&body body)
  (non-concept-class 'dsl::morph body))

(defmacro sense (&body body)
  (optionally-named-concept-subtype 'dsl::sense body)) ; TODO

