(in-package :dsl)

;;; old TRIPS lexicon/ontology loading
;; TODO make sure this actually treats things the same as old lex/ont, to the extent possible

(import '(common-lisp::in-package) :lexicon-data)

(defun convert-variables-to-disjunctions (x)
  "Convert things of the form (? var opt1 opt2) to (w::or opt1 opt2), and (?
   var) to t."
  (cond
    ((not (consp x))
      x)
    ((not (member (car x) '(? ld::? om::? lxm::?)))
      (mapcar #'convert-variables-to-disjunctions x))
    ((cddr x)
      (cons 'w::or (cddr x)))
    ((cdr x)
      t)
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
    ;; TODO handle :required and :default as in ONT::MOVE
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
	        (let* ((implements (second (assoc :implements params)))
		       (roles (if implements
		                (list role (util::convert-to-package implements :ONT))
				role))
		       (restr (convert-old-sem restr-sem))
		       )
		  `(,roles ,restr ,@(when (eq :optional optionality) '(ld::optional))))))
	    arguments))))
    ;; TODO coercions is only used once in the entire ontology, is it worth it?
    ))

;; TODO the use of variables in templates can be complicated, e.g. THEME-PRED-EXPERIENCER-OPTIONAL-TEMPL
(defmacro lxm::define-templates ((&rest templ-specs))
  `(progn
    (ld::provenance TRIPS)
    ,@(mapcar
      (lambda (templ-spec)
        (let ((name (util::convert-to-package (car templ-spec) :ONT))
	      (syn-feats
	        (convert-variables-to-disjunctions
		    (util::convert-to-package
		        (cdr (assoc 'lxm::syntax templ-spec)) :ld)))
	      (args (cdr (assoc 'lxm::arguments templ-spec)))
	      )
	  (if (find :parameter args :key #'caadr)
	    `(defun ,name ()
	       ; TODO
	       )
	    `(ld::syntax ,name
	      ,@(when syn-feats
	        `((ld::syn-feats ,@syn-feats)))
	      ,@(when args
	        `((ld::syn-sem
		  ,@(mapcar
		      (lambda (arg)
		        (destructuring-bind (syn-arg constit sem-role
			                     &optional optional) arg
			  `(,(util::convert-to-package syn-arg :ld)
			    ,(second constit) ; TODO
			    ,sem-role
			    ,@(when optional '(ld::optional))
			    )))
		      args))))
	      )
	    )))
      templ-specs
      )))

(defmacro ld::define-words (&key pos templ boost-word tags words)
  `(ld::morph (ld::pos ,(util::convert-to-package pos :ld))
    (ld::provenance TRIPS
      ,@(mapcar (lambda (tag) `(ld::provenance ,tag)) tags))
    ,@(mapcar
        (lambda (word-senses-spec)
	  (let ((word-spec (first word-senses-spec))
	        (wordfeats (cdr (assoc 'ld::wordfeats word-senses-spec)))
		(abbrev (cdr (assoc 'ld::abbrev word-senses-spec)))
		(sense-specs (cdr (assoc 'ld::senses word-senses-spec)))
		)
	    `(word ,word-spec
	      ,@(when wordfeats nil) ; TODO
	      ,@(when abbrev nil) ; TODO
	      ,@(mapcar
	          (lambda (sense-spec)
		    (let* ((parent (second (assoc 'ld::lf-parent sense-spec)))
		           (sense-templ (second (assoc 'ld::templ sense-spec))) ; TODO params
			   (sem-feats (cdr (assoc 'ld::sem sense-spec)))
			   (examples (mapcar #'second (remove-if-not (lambda (f) (eq 'ld::example (car f))) sense-spec)))
			   ;; TODO lf, lf-form, syntax (which includes morph), preference, non-hierarchy-lf, meta-data, prototypical-word
			   (effective-templ (or sense-templ templ))
			   )
		      `(ld::sense ; TODO :* name?
		        ,@(when sem-feats
			  `((ld::sem-feats
			     ,@(convert-variables-to-disjunctions
				 (util::convert-to-package sem-feats :ld)))))
			,@(when effective-templ
			  `((ld::inherit ,effective-templ)))
			,@(mapcar
			    (lambda (ex)
			      `(ld::example (ld::text ,ex)))
			    examples)
			)))
		  sense-specs)
	      )))
	words)
    ))

(defmacro ld::define-list-of-words ()
  ; TODO
  )

