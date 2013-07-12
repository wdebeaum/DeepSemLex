;;;; load.lisp - stuff to use for reading lexicon data; mostly macros

(in-package :dsl)

;;; lexical context

(defvar *concept-stack* nil)
(defun current-concept () (car *concept-stack*))
(defvar *current-provenance* nil)
(defvar *current-input-text* nil) ; not really used but set by non-concept-class
(defvar *current-word* nil)
(defvar *current-morph* nil)

(defun load-dsl-file (filename)
  (let (
        ;; reset lexical context
        *concept-stack*
        *current-provenance*
	*current-input-text*
	*current-word*
	*current-morph*
	;; make sure we read in the LD package
        (*package* (find-package :lexicon-data))
	)
  (load filename)))

;;; macro helper functions

(defun ld-to-dsl-package (s)
  "If s is a symbol in the lexicon-data package, convert it to the dsl package."
  (if (and (symbolp s) (eql (symbol-package s) (find-package :lexicon-data)))
    (intern (symbol-name s) :dsl)
    s))

(defun adjust-feature-packages (f)
  "Convert feature names and values to dsl package, and ORs to W package."
  (list (ld-to-dsl-package (first f))
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
	  (t (error "bogus feature: ~s" f))
	  )
	))

(defun concept-formula (x)
  "Convert symbols representing concepts in a concept formula like (w::and
   (w::or VN::foo WN::bar) FN::baz (concept PB::glarch ...)). Only recurses on
   ands and ors. Also wrap all of x in a let resetting certain lexical context."
  `(let ((*concept-stack* nil)
         (*current-word* nil)
	 (*current-morph* nil))
    ,(cond
      ((and (consp x) (member (util::convert-to-package (car x)) '(and or)))
	(cons (car x) (mapcar #'concept-formula (cdr x))))
      ((symbolp x) `(get-or-make-concept ',x))
      (t x)
      )))

(defmacro operator-cond ((op-var form-var body-forms &key (convert-operator-package t)) &body cases)
  "For each of the body-forms, if the operator satisfies one of the cases'
   tests, use the body of that case to convert body-form to an expression to
   evaluate. Otherwise keep the body-form unchanged."
  `(mapcar
       (lambda (,form-var)
	 (let ((,op-var ,(if convert-operator-package
	                   `(ld-to-dsl-package (car ,form-var))
			   `(car ,form-var))))
	   (cond
	     ,@cases
	     (t ,form-var)
	     )))
       ,body-forms))

(defun non-concept-class (cls body)
  "Return code to be evaluated to instantiate a non-concept class cls with the
   given body forms. Forms starting with a slot name set the slot to the second
   value in the form, while others are left to be evaluated normally (with
   *current-<cls>* set to the new instance)."
  (let ((slots (class-slot-names cls))
	(current-var (intern (concatenate 'string "*CURRENT-" (symbol-name cls) "*") :dsl)))
    `(progn
      (setf ,current-var (make-instance ',cls))
      (when (and (current-concept) (slot-exists-p (current-concept) ',cls))
	(setf (slot-value (current-concept) ',cls) ,current-var))
      ,@(operator-cond (operator form body)
	((member operator slots)
	  `(setf (slot-value ,current-var ',operator)
	         ',(ld-to-dsl-package (second form))))
	)
      ,current-var)))

(defun relation (label targets)
  "Return code to be evaluated to make relations with the given label from the
   current concept to the targets."
  `(let ((*current-provenance* *current-provenance*))
    ,@(mapcar
	(lambda (target)
	  (cond
	    ((and (listp target) (eq 'ld::provenance (car target)))
	      target)
	    (t
	      `(add-relation (current-concept) ',label ,(concept-formula target)
			     *current-provenance*))
	    ))
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
	    (*current-input-text*	*current-input-text*)
            (*current-word*		*current-word*)
	    (*current-morph*		*current-morph*)

            (anonymous ,(null name))
	    (outer-type (type-of (current-concept)))
	    (inner-part-of-outer (concept-part-of-p ',concept-type outer-type))
	    (outer-part-of-inner (concept-part-of-p outer-type ',concept-type))
	    )
      ;; From email "deep semantic lexicon model and file format" 2013-06-07:
      ;; Nested instantiations can have one of two meanings. If the inner class
      ;; is part of the outer one, it is taken to define that part of the outer
      ;; class (e.g. in (sense (sem-frame ...)), the sem-frame is part of the
      ;; sense). If the outer class is part of or the same as the inner one,
      ;; the inner instance gets the properties of the outer one (e.g. in (word
      ;; foo (morph ...)), the morph gets "foo" as the base form). This happens
      ;; via an inheritance relation if they're both concepts.
      (cond
	((not anonymous)
	  ;; separate the inner named concept from its outer concept, and link
	  ;; them with an inheritance relation
	  (push (get-or-make-concept ',name ',concept-type)
		*concept-stack*)
	  (cond
	    ((null (second *concept-stack*))
	      nil)
	    (outer-part-of-inner
	      (add-relation 
		  (current-concept) :inherit (second *concept-stack*)
		  *current-provenance*
		  ))
	    (inner-part-of-outer
	      (add-relation 
		  (second *concept-stack*) :inherit (current-concept)
		  *current-provenance*
		  ))
	    )
	  )
	((null *concept-stack*)
	  ;; no outer concept, just make the (anonymous) inner one
	  (push (make-instance ',concept-type) *concept-stack*)
	  )
	(outer-part-of-inner
	  ;; outer concept is same type or part of anonymous inner concept,
	  ;; inner is separate from but inherits outer
	  (push (make-instance ',concept-type) *concept-stack*)
	  (add-relation
	      (current-concept) :inherit (second *concept-stack*)
	      *current-provenance*
	      )
	  )
	(inner-part-of-outer
	  ;; anonymous inner concept is strictly part of outer concept,
	  ;; get or make the part of outer that inner is, and put it on the
	  ;; stack
	  (push (get-or-make-part-of ',concept-type (current-concept))
	        *concept-stack*))
	(t
	  (error "incompatible nested concept types: ~s inside ~s" ',concept-type outer-type))
	)
      (unless (or (null *current-provenance*)
                  (member *current-provenance*
		          (provenance (current-concept))
			  :test #'eq))
        (push *current-provenance* (provenance (current-concept))))
      ;; mostly just keep body the way it is, but special case w::or forms
      ;; FIXME kind of a kludge... maybe ORs should go on *concept-stack*?
      ,@(mapcar
          (lambda (body-form)
	    (if (and (listp body-form) (eq 'w::or (car body-form)))
	      `(add-relation (current-concept) :inherit
		   (let (*concept-stack*
			 *current-word*
			 *current-morph*)
		     ,body-form))
	      body-form))
	  body)
      (current-concept))
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
  ;; convert initial symbol to `(name ,symbol)
  (when (symbolp (car body))
    (setf body
          (cons (list 'lexicon-data::name
	              (intern (symbol-name (car body)) :dsl))
	        (cdr body))
	  ))
  (non-concept-class 'provenance body))

(defmacro ld::definition (&body body)
  `(let ((*current-provenance* *current-provenance*))
     ,(non-concept-class 'input-text body)
     (unless (slot-value *current-input-text* 'provenance)
       (setf (provenance *current-input-text*) *current-provenance*))
     (push *current-input-text* (definitions (current-concept)))
     *current-input-text*))

(defmacro ld::example (&body body)
  `(let ((*current-provenance* *current-provenance*))
     ,(non-concept-class 'input-text body)
     (unless (slot-value *current-input-text* 'provenance)
       (setf (provenance *current-input-text*) *current-provenance*))
     (push *current-input-text* (examples (current-concept)))
     *current-input-text*))

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
  (optionally-named-concept-subtype 'sem-feats
      (operator-cond (operator form body)
        ((typep operator 'sem-feat)
	  `(push ',(adjust-feature-packages form) (features (current-concept)))
	  ))))

(defmacro ld::entailments (&body body)
  (optionally-named-concept-subtype 'entailments
      (operator-cond (operator form body :convert-operator-package nil)
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
  (optionally-named-concept-subtype 'syn-feats
      (operator-cond (operator form body)
        ((typep operator 'syn-feat)
	  `(push ',(adjust-feature-packages form) (features (current-concept)))
	  ))))

(defmacro ld::syntax (&body body)
  (optionally-named-concept-subtype 'syntax body))

(defmacro ld::word (word-spec &body body)
  `(let ((*current-word* (make-word-from-spec ',word-spec)))
    ,@body
    (when (and *current-morph* (not (maps *current-morph*)))
      (add-morph-maps-for-word *current-morph* *current-word*))
    *current-word*))

(defmacro ld::morph (&body body)
  (non-concept-class 'morph body))

(defmacro ld::sense (&body body)
  `(let ((s ,(optionally-named-concept-subtype 'sense body)))
    ;; add the current morph if the sense doesn't already have one
    (when (and *current-morph* (not (slot-boundp s 'morph)))
      (setf (morph s) *current-morph*))
    ;; if the sense now has a morph, add it to (senses *db*)
    (when (slot-boundp s 'morph)
      ;; add the current word to the morph if it doesn't already have maps
      (unless (maps *current-morph*)
        (add-morph-maps-for-word *current-morph* *current-word*))
      (add-morphed-sense-to-db *db* s))
    s))

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

