;(dolist (p '(w f ont fn pb wn vn))
;  (eval `(defpackage ,p)))

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up :up "config" "lisp")
                       :name "trips")))
(load #!TRIPS"src;DeepSemLex;code;lib;defsys")

(in-package :dsl)

(defvar *indent* 0)

(defmacro indented (&body body)
  `(let ((*indent* (1+ *indent*)))
    ,@body))

(load #!TRIPS"src;DeepSemLex;code;converters;lf-to-rdf")
(load #!TRIPS"src;DeepSemLex;code;converters;syntax-tree-to-xml")

(defvar *xml-escapes* '((#\" "quot") (#\< "lt") (#\> "gt") (#\& "amp")))

(defun xml-escape (str)
  "Replace characters special to XML with their corresponding entities, as
   listed in the *xml-escapes* alist."
  (loop with start-pos = 0
	for pos =
	  (position-if (lambda (c) (assoc c *xml-escapes*)) str
	      :start start-pos)
	while pos
	nconcing 
	       (list (subseq str start-pos pos)
		     "&" (second (assoc (elt str pos) *xml-escapes*)) ";")
	into chunks
	do (setf start-pos (1+ pos))
	finally (return (apply #'concatenate `(string ,@chunks ,(subseq str start-pos))))
	))

(defmacro concept-element ((xml tag-name op-var form-var body-forms) &body cases)
  `(let ((body-forms ,body-forms))
    (format ,xml "~&~vt<~(~a~)" *indent* ,tag-name)
    (when (or (symbolp (car body-forms)) (trips-sense-name-p (car body-forms)))
      (format xml " name=\"~(~s~)\"" (pop body-forms)))
    (let (aliases other-forms)
      (loop for f in body-forms
	    do
	      (if (member (car f) '(alias aliases))
	        (setf aliases (append aliases (cdr f)))
		(push f other-forms)
		))
      (when aliases
	(format ,xml " aliases=\"~(~{~s~^ ~}~)\"" aliases))
      (cond
        (other-forms
          (format ,xml ">")
	  (indented
	    (loop for ,form-var in (reverse other-forms)
		  for ,op-var = (when (consp ,form-var) (car ,form-var))
		  do
		    (cond
		      ,@cases
		      (t (dsl-to-xml-stream ,form-var ,xml))
		      )
		  ))
	  (format ,xml "~&~vt</~(~a~)>" *indent* ,tag-name)
	  )
	(t ; no other-forms
	  (format ,xml " />"))
	)
      )
    ))

(defun optional-body (xml tag-name body)
  (cond
    (body
      (format xml ">")
      (indented
	(dolist (c body)
	  (dsl-to-xml-stream c xml)))
      (format xml "~&~vt</~(~a~)>" *indent* tag-name)
      )
    (t
      (format xml " />"))
    ))

;; TODO move xml parameter to first position since that's where it is everywhere else
(defun dsl-to-xml-stream (dsl xml)
  (ecase (car dsl)
    (word
      ;; TODO now that we're loading the lib anyway, change this to actually make a word object
      (cond
        ((symbolp (second dsl))
	  (format xml "~&~vt<word first-word=\"~(~s~)\"" *indent* (second dsl)))
	((not (listp (second dsl)))
	  (error "bogus word spec; expected symbol or list, but got: ~s" (second dsl)))
	((every #'symbolp (second dsl))
	  (format xml "~&~vt<word first-word=\"~(~s~)\" remaining-words=\"~(~{~s~^ ~}~)\"" *indent* (car (second dsl)) (cdr (second dsl))))
        ((and (every #'symbolp (butlast (second dsl)))
	      (typep (car (last (second dsl))) '(cons symbol null)))
	  (format xml "~&~vt<word first-word=\"~(~s~)\" remaining-words=\"~(~{~s~^ ~}~)\" particle=\"~(~s~)\"" *indent* (car (second dsl)) (butlast (cdr (second dsl))) (caar (last (second dsl)))))
	(t
	  (error "bogus word spec; expected list of symbols with possible final list of one particle symbol, but got: ~s" (second dsl)))
	)
      (optional-body xml "word" (cddr dsl))
      )
    (pos
      (format xml "~&~vt<pos pos=\"~(~s~)\"" *indent* (second dsl))
      (optional-body xml "pos" (cddr dsl))
      )
    (forms
      (format xml "~&~vt<forms>" *indent*)
      (indented
	(dolist (f (cdr dsl))
	  (etypecase f
	    (list
	      (format xml "~&~vt<~(~s~)>" *indent* (repkg (car f)))
	      (indented (dsl-to-xml-stream (cons 'word (cdr f)) xml))
	      (format xml "~&~vt</~(~s~)>" *indent* (repkg (car f)))
	      )
	    (symbol
	      (format xml "~&~vt~a" *indent* f))
	    )))
      (format xml "~&~vt</forms>" *indent*)
      )
    (provenance
      (let ((attributes (cdr dsl)) children)
        (when (symbolp (car attributes))
	  (setf attributes
	        (cons (list 'name (car attributes)) (cdr attributes))))
	(setf attributes
	      (remove-if
	          (lambda (form)
		    (when (eq 'provenance (car form))
		      (push form children)
		      t))
		  attributes))
        (format xml "~&~vt<provenance~{ ~(~s~)=\"~a\"~}" *indent* (apply #'append attributes))
	(cond
	  (children
	    (format xml ">")
	    (indented
	      (dolist (child children)
	        (dsl-to-xml-stream child xml)))
	    (format xml "~&~vt</provenance>" *indent*)
	    )
	  (t
	    (format xml "/>"))
	  )
	))
    ((concept syntax semantics sense)
      (concept-element (xml (car dsl) operator form (cdr dsl))
        ; no special cases
	))
    (sem-frame
      (concept-element (xml (car dsl) operator form (cdr dsl))
	((typep operator '(or sem-role (and cons (list-of sem-role))))
	  (destructuring-bind (roles restriction &optional optional) form
	    (format xml "~&~vt<role-restr-map roles=\"~(~{~s~^ ~}~)\"~:[~; optional=\"optional\"~]>" *indent* (if (listp roles) roles (list roles)) optional)
	    (indented
	      (etypecase restriction
		(symbol (format xml "~(~s~)" restriction))
		(list (dsl-to-xml-stream restriction xml))
		))
	    (format xml "~@[~&~vt~]</role-restr-map>" (when (listp restriction) *indent*))
	    )
	    )))
    (syn-sem
      (concept-element (xml (car dsl) operator form (cdr dsl))
	((typep operator 'syn-arg)
	  (destructuring-bind (syn-arg syn-cat &optional sem-role optional) form
	    (format xml "~&~vt~(<syn-sem-map syn-arg=\"~s\" syn-cat=\"~s\"~@[ head-word=\"~s\"~]~@[ sem-role=\"~s\"~]~:[~; optional=\"optional\"~]/>~)"
		*indent*
		syn-arg
		(if (listp syn-cat) (car syn-cat) syn-cat)
		(when (listp syn-cat) (second syn-cat))
		sem-role
		optional
		)))))
    ((syn-feats sem-feats)
      (concept-element (xml (car dsl) operator form (cdr dsl))
	((typep operator '(or syn-feat sem-feat))
	  (format xml "~&~vt<feat name=\"~(~s~)\">" *indent* operator)
	  (etypecase (second form)
	    (symbol
	      (format xml "~(~s~)</feat>" (second form)))
	    (list
	      (dsl-to-xml-stream (second form) xml)
	      (format xml "~&~vt</feat>" *indent*)
	      )
	    )
	  )))
    (entailments
      (concept-element (xml (car dsl) operator form (cdr dsl))
        ((stringp form)
	  (format xml "~&~vt~a" *indent* (xml-escape form))
	  )))
    ((definition example)
      (let ((text (second (assoc 'text (cdr dsl)))))
        (when text (setf text (xml-escape text)))
        (format xml "~&~vt<~(~s~)~@[ text=~s~]>" *indent* (car dsl) text))
      (indented
        (dolist (f (cdr dsl))
	  ;; I would've put these cases up among the others, but I decided to
	  ;; move lf-root from the input-text to the lf-terms element, since
	  ;; that's where it is in the WebParser output
	  (case (car f)
	    ((text lf-root)
	      nil)
	    (lf-terms
	      (format xml "~&~vt<lf-terms root=\"~a\">" *indent* (second (assoc 'lf-root (cdr dsl))))
	      (indented (lf-to-rdf-stream (cdr f) xml))
;	      (indented
;		(dolist (term (cdr f))
;		  (format xml "~&~vt~s" *indent* term)))
	      (format xml "~&~vt</lf-terms>" *indent*)
	      )
	    (syntax-tree
	      (format xml "~&~vt<syntax-tree>" *indent*)
	      (indented (syntax-tree-to-xml-stream (second f) xml))
;	      (indented
;	        (let ((*print-pretty* t))
;		  (format xml "~&~vt~s" *indent* (second f))))
	      (format xml "~&~vt</syntax-tree>" *indent*)
	      )
	    (lattice
	      (format xml "~&~vt<lattice>" *indent*)
	      (indented
		(dolist (term (cdr f))
		  (format xml "~&~vt~s" *indent* term)))
	      (format xml "~&~vt</lattice>" *indent*)
	      )
	    (otherwise
	      (dsl-to-xml-stream f xml))
	    )))
      (format xml "~&~vt</~(~s~)>" *indent* (car dsl))
      )
    (>
      (format xml "~&~vt<relation label=\"~(~s~)\">" *indent* (second dsl))
      (indented
        (dolist (f (cddr dsl))
	  (cond
	    ((listp f)
	      (dsl-to-xml-stream f xml))
	    ((and (symbolp f)
	          (every
		    (lambda (c)
		      (or (char= #\-) (digit-char-p c) (upper-case-p c)))
		    (symbol-name f))
		  )
	      (format xml "~&~vt~(~s~)" *indent* f))
	    (t
	      (format xml "~&~vt~s" *indent* f))
	    )))
      (format xml "~&~vt</relation>" *indent*)
      )
    ((inherit overlap subtype-of)
      (dsl-to-xml-stream (cons '> dsl) xml))
    ((w::and w::or and or morph)
      (format xml "~&~vt<~(~s~)>" *indent* (repkg (car dsl)))
      (indented
	(dolist (f (cdr dsl))
	  (cond
	    ((listp f)
	      (dsl-to-xml-stream f xml))
	    ((symbolp f)
	      (format xml "~&~vt~(~s~)" *indent* f))
	    (t
	      (format xml "~&~vt~a" *indent* (xml-escape (format nil "~s" f))))
	    )))
      (format xml "~&~vt</~(~s~)>" *indent* (repkg (car dsl)))
      )
    ))

(defun cl-user::run ()
  (format *standard-output* "~&<?xml version=\"1.0\"?>~&<dsl>")
  (loop with *package* = (find-package :dsl)
        for expr = (read *standard-input* nil) while expr
        do (indented (dsl-to-xml-stream expr *standard-output*)))
  (format *standard-output* "~&</dsl>~%")
  )

