;;;; data.lisp - stuff to use for reading lexicon data; mostly macros

(in-package :dsl)

;;; lexical context

(defvar *concept-stack* nil)
(defun current-concept () (car *concept-stack*))
(defvar *current-provenance* nil)
(defvar *current-word* nil)
(defvar *current-morph* nil)

;;; macro helper functions

; seems unsafe to apply generally...
;(defun adjust-symbol-packages (x)
;  "Change the package of lexicon-data symbols not in operator position to dsl."
;  (cond
;    ((consp x)
;      (cons (car x) (mapcar #'adjust-symbol-packages (cdr x))))
;    ((and (symbolp x) (eq (symbol-package x) (find-package :lexicon-data)))
;      (intern (symbol-name x) :dsl))
;    (t x)
;    ))

(defun adjust-feature-packages (features)
  "Convert feature names and values to dsl package, and ORs to W package."
  (mapcar
      (lambda (f)
        (list (intern (symbol-name (first f)) :dsl)
	      (cond
	        ((symbolp (second f))
		  (intern (symbol-name (second f)) :dsl))
		((and (consp (second f))
		      (every #'symbolp (second f))
		      (string= "OR" (symbol-name (first (second f)))))
		  (cons 'w::or
		      (mapcar
		          (lambda (s)
			    (intern (symbol-name s) :dsl))
			  (cdr (second f))
			  )))
		(t (error "bogus feature list: ~s" features))
		)
	      ))
      features))

(defun concept-formula (x)
  "Convert symbols representing concepts in a concept formula like (w::and
   (w::or VN::foo WN::bar) FN::baz (concept PB::glarch ...)). Only recurses on
   ands and ors."
  (cond
    ((and (consp x) (member (util::convert-to-package (car x)) '(and or)))
      (cons (car x) (mapcar #'concept-formula (cdr x))))
    ((symbolp x)
      `(get-or-make-concept ',x))
    (t x)
    ))

(defmacro operator-cond ((op-var form-var body-forms) &body cases)
  "For each of the body-forms, if the operator satisfies one of the cases'
   tests, use the body of that case to convert body-form to an expression to
   evaluate. Otherwise keep the body-form unchanged."
  `(mapcar
      (lambda (body-form)
	(loop with operator = (car body-form)
	      with op-var = ',op-var
	      with form-var = ',form-var
	      for cs in ',cases
	      when (eval `(let ((,op-var ',operator)) ,(car cs)))
		return
		 (eval
		    `(let ((,op-var ',operator)
		           (,form-var ',body-form)
			   )
		      ,@(cdr cs)))
	      finally (return `',body-form)
	      ))
      ,body-forms))

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

(defun relation (label targets)
  "Return code to be evaluated to make relations with the given label from the
   current concept to the targets."
  `(progn
    ,@(mapcar
	(lambda (target)
	  `(add-relation (current-concept) ',label ,(concept-formula target)
	  		 *current-provenance*))
	targets)
    ))

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
	    (inner-part-of-outer (subtypep outer-type ',concept-type))
	    (outer-part-of-inner (subtypep ',concept-type outer-type))
	    )
      (cond
	((not anonymous)
	  (push (get-or-make-concept ',name ',concept-type)
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
	  (error "incompatible nested concept types: ~s inside ~s" ',concept-type outer-type))
	)
      (unless (or (null *current-provenance*)
                  (member *current-provenance*
		          (provenance (current-concept))
			  :test #'eq))
        (push *current-provenance* (provenance (current-concept))))
      ,@body
      )
    ))

(defun make-word-from-spec (spec)
  "Make an instance of the word class from a specification like the
  following:
    singleword
    still-a-single-word
    (multiple contiguous words)
    (word with (particle))
  "
  (setf spec (convert-to-package spec :w))
  (cond
    ((symbolp spec)
      (make-instance 'word
          :first-word spec))
    ((not (listp spec))
      (error "bogus word spec; expected symbol or list, but got: ~s" spec))
    ((every #'symbolp spec)
      (make-instance 'word
          :first-word (car spec)
	  :remaining-words (cdr spec)
	  ))
    ((and (every #'symbolp (butlast spec))
          (typep (car (last spec)) '(cons symbol null)))
      (make-instance 'word
          :first-word (car spec)
	  :remaining-words (butlast (cdr spec))
	  :particle (caar (last spec))
	  ))
    (t
      (error "bogus word spec; expected list of symbols with possible final list of one particle symbol, but got: ~s" spec))
    ))

;;; constructor/reference macros

(defmacro ld::provenance (&body body)
  (non-concept-class 'provenance body))

(defmacro ld::concept (&body body)
  (optionally-named-concept-subtype 'concept body))

(defmacro ld::sem-frame (&body body)
  (optionally-named-concept-subtype 'sem-frame
      (operator-cond (operator form body)
        ((typep operator '(or sem-role (list-of sem-role)))
	  (destructuring-bind (roles restriction &optional optional) form
	    `(push 
		(make-instance 'role-restr-map
		    :roles ',(util::convert-to-package (if (listp roles) roles (list roles)) :dsl)
		    :restriction ,(concept-formula restriction)
		    :optional ,(not (null optional))
		    )
		(maps (current-concept))
		)))
	)
      ))

(defmacro ld::sem-feats (&body body)
  `(setf (sem-feats (current-concept)) ',(adjust-feature-packages body)))

(defmacro ld::entailments (&body body)
  (optionally-named-concept-subtype 'sem-frame
      (operator-cond (operator form body)
        ((and (symbolp operator) (not (eql (symbol-package operator) (find-package :lexicon-data))))
	  ;; TODO something better
	  `(push ',form (terms (current-concept))))
	)))

(defmacro ld::semantics (&body body)
  (optionally-named-concept-subtype 'semantics body))

(defmacro ld::syn-sem (&body body)
  (optionally-named-concept-subtype 'syn-sem
      (operator-cond (operator form body)
        ((typep operator 'syn-arg)
	  (destructuring-bind (syn-arg syn-cat &optional sem-role optional) form
	    `(push
		(make-instance 'syn-sem-map
		    :syn-arg ',(util::convert-to-package syn-arg :dsl)
		    :syn-cat ',(util::convert-to-package (if (listp syn-cat) (car syn-cat) syn-cat) :dsl)
		    :head-word ',(when (listp syn-cat) (util::convert-to-package (second syn-cat) :w))
		    :sem-role ',(util::convert-to-package sem-role :dsl)
		    :optional ,(not (null optional))
		    )
		(maps (current-concept))
		)))
	)))

(defmacro ld::syn-feats (&body body)
  `(setf (syn-feats (current-concept)) ',(adjust-feature-packages body)))

(defmacro ld::syntax (&body body)
  (optionally-named-concept-subtype 'syntax body))

(defmacro ld::word (word-spec &body body)
  `(let ((*current-word* (make-word-from-spec ',word-spec)))
    ,@body))

(defmacro ld::morph (&body body)
  (non-concept-class 'morph body))

(defmacro ld::sense (&body body)
  (optionally-named-concept-subtype 'sense body)) ; TODO

;;; relation macros

(defmacro ld::inherit (&body body) (relation :inherit body))
(defmacro ld::overlap (&body body) (relation :overlap body))
(defmacro ld::subtype-of (&body body) (relation :subtype-of body))

;;; boolean formula literals

(defmacro w::and (&body body)
  `(list 'w::and ,@body))

(defmacro w::or (&body body)
  `(list 'w::or ,@body))

(import '(w::and w::or) :lexicon-data)

