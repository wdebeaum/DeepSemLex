(in-package :dsl)

;;; loading of old TRIPS lexicon/ontology
;; TODO make sure this actually treats things the same as old lex/ont, to the extent possible

(import '(common-lisp::in-package) :lexicon-data)

(defun convert-variables-to-disjunctions (x)
  "Convert things of the form (? var opt1 opt2) to (w::or opt1 opt2),
   (? var foo) to just foo, and (? var) to t."
  (cond
    ((not (consp x))
      x)
    ((not (member (car x) '(? ld::? om::? lxm::?)))
      (mapcar #'convert-variables-to-disjunctions x))
    ((= 2 (length x))
      t)
    ((= 3 (length x))
      (third x))
    ((< 3 (length x))
      (cons 'w::or (cddr x)))
    (t (mapcar #'convert-variables-to-disjunctions x))
    ))

(defun convert-old-sem (old-sem &key part-p)
  "Convert an old sem feature list to an expression to evaluate to get a
   corresponding concept."
  (let* ((old-sem-disj
           (convert-variables-to-disjunctions
	       (util::convert-to-package old-sem :ld)))
         (old-fl-type (car old-sem-disj))
	 (old-fl (cdr old-sem-disj))
	 )
    ;; for now just flatten :required and :default
    (setf old-fl
          (mapcan
	      (lambda (x)
	        (if (member (car x) '(:required :default))
		  (cdr x)
		  (list x)
		  ))
	      old-fl))
    (cond
      ((and old-fl-type old-fl)
          `(ld::sem-feats (ld::inherit ,old-fl-type) ,@old-fl))
      (old-fl-type
        (if part-p
	  `(ld::inherit ,old-fl-type)
	  old-fl-type))
      (old-fl
        `(ld::sem-feats ,@old-fl))
      (t 'ld::t)
      )))

(defmacro om::define-type (type &key parent sem arguments coercions wordnet-sense-keys)
  `(ld::concept ,type
    (ld::provenance TRIPS)
    ,@(when parent
      `((ld::inherit ,parent)))
    ,@(when wordnet-sense-keys
      `((ld::overlap ,@(mapcar (lambda (sk-str) (intern sk-str :WN)) wordnet-sense-keys))))
    ,@(when sem
      (list (convert-old-sem sem :part-p t)))
    ,@(when arguments
      `((ld::sem-frame
        ,@(mapcar
	    (lambda (arg)
	      (destructuring-bind (optionality role &optional restr-sem &rest params) arg
		(setf role (util::convert-to-package role :ONT))
	        (let* ((implements (util::convert-to-package (second (assoc :implements params)) :ONT))
		       (roles (if (and implements (not (eq implements role)))
		                (list role implements)
				role))
		       (restr (convert-old-sem restr-sem))
		       )
		  `(,roles ,restr ,@(when (eq :optional optionality) '(ld::optional))))))
	    arguments))))
    ;; TODO coercions is only used once in the entire ontology, is it worth it?
    ))

(defun get-syn-cat-from-constit (constit)
  "Get syn-cat and optionally head words from a (% syn-cat . features) constit."
  (let ((syn-cat (second constit))
        (head-words
	  (mapcan
	      (lambda (x)
		(if (consp (second x)) ; (? var opt1 opt2...)
		  (cddr (second x))
		  (cdr x)
		  ))
	      (remove-if-not
		  (lambda (x)
		    (member (car x) '(w::ptype w::lex)))
		  (cddr constit)
		  )
	      )))
    ;; FIXME what about other features?
    (cond
      ((null head-words)
        syn-cat)
      ((= 1 (length head-words))
        (cons syn-cat head-words))
      (t
        (list syn-cat (cons 'ld::or head-words)))
      )))

(defun get-defun-params-from-templ-args (args)
  (mapcan
      (lambda (arg)
        (when (eq :parameter (caadr arg))
	  (let ((param-name (second (second arg)))
	        (default (assoc :default (cddr (second arg)))))
	    (if default
	      `((,param-name ',(get-syn-cat-from-constit (second default))))
	      (list param-name)
	      ))))
      args))

;; TODO the use of variables in templates can be complicated, e.g.
;; THEME-PRED-EXPERIENCER-OPTIONAL-TEMPL
;; AFFECTED-COST-COMPLEX-SUBJCONTROL-TEMPL
;; ...
(defmacro lxm::define-templates ((&rest templ-specs))
  `(progn
    (ld::provenance TRIPS)
    ,@(mapcar
      (lambda (templ-spec)
        (let ((name (util::convert-to-package (car templ-spec) :ONT))
	      (syn-feats
	        (convert-variables-to-disjunctions
		    (util::convert-to-package
		        (cdr (assoc 'lxm::syntax (cdr templ-spec))) :ld)))
	      (args (cdr (assoc 'lxm::arguments (cdr templ-spec))))
	      )
	  (setf syn-feats
	        (delete-if
		    (lambda (x)
		      (member (car x) '(
		        ld::morph ; TODO handle morph separately
			ld::arg ld::sa-id ; just variables, useless
			ld::sem ; not syntactic
			ld::qcomp ld::qof ; constit-valued (not symbols)
			)))
		    syn-feats))
	  (if (find :parameter args :key #'caadr)
	    `(defun ,name (&key ,@(get-defun-params-from-templ-args args))
	      (ld::syntax
		,@(when syn-feats
		  `((ld::syn-feats ,@syn-feats)))
		;; see also ld::syn-sem in load.lisp
		,(optionally-named-concept-subtype 'syn-sem
		    (mapcar
		      (lambda (arg)
			(destructuring-bind (syn-arg constit sem-role
					     &optional optional) arg
			  `(let ((syn-cat
			           ,(if (eq :parameter (car constit))
			             (second constit)
			             `',(get-syn-cat-from-constit constit)
			             )))
			    (push
				(make-instance 'syn-sem-map
				    :syn-arg ',(util::convert-to-package syn-arg :dsl)
				    :syn-cat (util::convert-to-package (if (listp syn-cat) (car syn-cat) syn-cat) :dsl)
				    :head-word (when (listp syn-cat) (util::convert-to-package (second syn-cat) :w))
				    :sem-role ',sem-role
				    :optional ,(not (null optional))
				    )
				(maps (current-concept))
				))))
		      args))
	        ))
	    `(progn
	      (ld::syntax ,name
		,@(when syn-feats
		  `((ld::syn-feats ,@syn-feats)))
		,@(when args
		  `((ld::syn-sem
		    ,@(mapcar
			(lambda (arg)
			  (destructuring-bind (syn-arg constit sem-role
					       &optional optional) arg
			    `(,(util::convert-to-package syn-arg :ld)
			      ,(get-syn-cat-from-constit constit)
			      ,sem-role
			      ,@(when optional '(ld::optional))
			      )))
			args))))
		)
	      ;; define it as a function anyway so we can always use templates
	      ;; the same way
	      (defun ,name () (gethash ',name (concepts *db*)))
	      )
	    )))
      templ-specs
      )))

(defun convert-templ-call (templ-call)
  (cons (util::convert-to-package (car templ-call) :ONT)
        (mapcan
	    (lambda (arg)
	      `(,(intern (symbol-name (car arg)) :keyword)
	        ',(get-syn-cat-from-constit
		    (second arg))))
	    (cdr templ-call))
        ))

(defmacro ld::define-words (&key pos templ boost-word tags words)
  (setf templ (list (util::convert-to-package templ :ONT)))
  `(progn
    (ld::pos ,(util::convert-to-package pos :ld))
    (ld::provenance TRIPS
      ,@(mapcar (lambda (tag) `(ld::provenance ,tag)) tags))
    ,@(mapcar
        (lambda (word-senses-spec)
	  (let ((word-spec (first word-senses-spec))
	        (wordfeats (cdr (assoc 'ld::wordfeats (cdr word-senses-spec))))
		(abbrev (cdr (assoc 'ld::abbrev (cdr word-senses-spec))))
		(sense-specs (cdr (assoc 'ld::senses (cdr word-senses-spec))))
		)
	    `(ld::word ,word-spec
	      ,@(when wordfeats nil) ; TODO
	      ,@(when abbrev nil) ; TODO
	      ,@(mapcar
	          (lambda (sense-spec)
		    (let* ((parent (second (assoc 'ld::lf-parent sense-spec)))
		           (sense-templ (cdr (assoc 'ld::templ sense-spec)))
			   (sem-feats (cdr (assoc 'ld::sem sense-spec)))
			   (examples (mapcar #'second (remove-if-not (lambda (f) (eq 'ld::example (car f))) sense-spec)))
			   ;; TODO lf, lf-form, syntax (which includes morph), preference, non-hierarchy-lf, meta-data, prototypical-word
			   (effective-templ
			     (if sense-templ
			       (convert-templ-call sense-templ)
			       templ
			       ))
			   )
		      `(ld::sense ; TODO :* name?
			(ld::inherit ,parent)
		        ,@(when sem-feats
			  `((ld::semantics ; FIXME icky extra level
			      (ld::sem-feats
			        ,@(convert-variables-to-disjunctions
				    (util::convert-to-package sem-feats :ld)
				    )))))
			,@(when effective-templ
			  `((add-relation (current-concept) :inherit
			      (let (*concept-stack*
			            *current-word*
				    *current-pos*
				    *current-morph*)
			        ,effective-templ)
			      *current-provenance*)))
			,@(mapcar
			    (lambda (ex)
			      `(ld::example (ld::text ,ex)))
			    examples)
			)))
		  sense-specs)
	      )))
	words)
    ))

