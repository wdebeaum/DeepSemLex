(defpackage :ont)
(defpackage :w)
(defpackage :f)
(defpackage :lexiconmanager) ; ugh.
(defpackage :wn)

;; ick.
(defpackage :deepsemlex (:nicknames :dsl) (:use common-lisp))
(load "../lib/mop")

(defun repkg (sym &optional (new-pkg *package*))
  (intern (symbol-name sym) new-pkg))

(defun strs-to-wn-sk-syms (x)
  (cond
    ((consp x)
      (cons (strs-to-wn-sk-syms (car x)) (strs-to-wn-sk-syms (cdr x))))
    ((stringp x)
      (intern x :wn))
    (t x)
    ))

(defun wnsenses-to-syms (lf-terms)
  "Destructively convert strings in :WNSENSE arguments to sense key symbols."
  (mapcar
      (lambda (lf-term)
        (let ((wnsense-rest (member :wnsense lf-term)))
	  (when wnsense-rest
	    (setf (cadr wnsense-rest)
	          (strs-to-wn-sk-syms (cadr wnsense-rest)))))
	lf-term)
      lf-terms))

(defvar *punc* '(
  (w::punc-colon ":")
  (w::punc-comma ",")
  (w::punc-exclamation-mark "!")
  (w::punc-minus "-")
;  (w::punc-ordinal "th") need to handle this separately
  (w::punc-period ".")
  (w::punc-question-mark "?")
  ))

;; TODO use :start, :end, and (third (:* ONT W)) instead of :words, so that spacing is right?
(defun words-to-str (words)
  "Convert a list of W:: words to an approximation of the original string they
   came from (no package prefixes, lower case, and with punctuation converted)."
  (let (prev-word)
    (substitute #\' #\^
	(string-downcase
	    (format nil "~{~a~^ ~}"
		(mapcar
		    (lambda (word)
		      (let ((converted-word (second (assoc word *punc*))))
		        (when (eq 'w::punc-ordinal word)
			  (setf converted-word
			    (if (integerp prev-word)
			      (case (abs (rem prev-word 10))
				(2 "nd")
				(3 "rd")
				(otherwise "th")
				)
			      "th"
			      )))
		          (setf prev-word word)
			  (or converted-word word)
			  ))
		    words))))))

(defun role-to-role-restr-map (role)
  (cond ; work around odd variations in restriction format
    ;; extra parens
    ((and (= 1 (length (second role))) (listp (first (second role))))
      (warn "extra parens in role restriction")
      (role-to-role-restr-map (list (first role) (first (second role)))))
    ;; no actual restriction
    ((null (second role))
      (warn "missing role restriction")
      `( ,(repkg (first role) :ont) t ))
    ;; the way it's supposed to be
    ((= 2 (length (second role)))
      (destructuring-bind (role-name (trips-class wn-class) syn-cats) role
          (declare (ignore syn-cats))
	`( ,(repkg role-name :ont)
	   ,(if wn-class
	     `(and ,trips-class ,(strs-to-wn-sk-syms wn-class))
	     trips-class)
	   )))
    (t (error "bogus role restriction: ~s" role))
    ))

(defun guess-syn-args (pos syn-cats)
  (let ((syn-args (mapcar (lambda (x) 'lcomp) syn-cats)))
    ;; guess syn-args to go with syn-cats and pos
    (ecase pos
      (v
	(unless syn-cats
	  (error "0-argument verb?!"))
	(setf (first syn-args) 'lsubj)
	(when (second syn-cats)
	  (setf (second syn-args)
	    (if (equalp '(NP) (second syn-cats))
	      'lobj
	      'lcomp
	      )))
	(when (third syn-cats)
	  (setf (third syn-args)
	    (if (equalp '(NP) (second syn-cats))
	      'liobj ;; FIXME assumes a lot
	      'lcomp
	      )))
	)
      (adj
	(when (first syn-cats)
	  (setf (first syn-args) 'argument))
	(when (second syn-cats)
	  (setf (second syn-cats)
		(if (equalp '(NP) (second syn-cats))
		  'premod 'post-subcat)))
	(when (third syn-cats)
	  (error "don't know what to do with 3-argument adjective!"))
	)
      ; TODO n, ...
      )
    syn-args))

(defun roles-to-syn-sem (pos roles)
  (let ((role-names (mapcar (lambda (role) (repkg (first role) :ont)) roles))
        syn-catses)
    (loop for role in roles
	  for syn-cats = (third role)
	  for syn-cat-to-head-words = nil
	  do
	    (dolist (syn-cat syn-cats)
	      (etypecase syn-cat
		(symbol
		  (pushnew (list syn-cat) syn-cat-to-head-words
			   :test #'equalp))
		(cons
		  (let ((already (assoc (car syn-cat) syn-cat-to-head-words)))
		    (if already
		      (when (cdr already)
			(setf (cdr already)
			      (union (cdr already) (cdr syn-cat))))
		      (push (copy-list syn-cat) syn-cat-to-head-words)
		      )))
		))
	    (push syn-cat-to-head-words syn-catses)
	  )
    (setf syn-catses (nreverse syn-catses))
    (let ((syn-sems
	    (mapcar
	      (lambda (syn-cats)
		`(syn-sem
		   ,@(loop with syn-args = (guess-syn-args pos syn-cats)
			   for role-name in role-names
			   for syn-cat in syn-cats
			   for syn-arg in syn-args
			   collect
			     (list syn-arg
				   (if (= 1 (length syn-cat))
				      (first syn-cat)
				      syn-cat)
				   role-name
				   )))
		  )
	      (dsl::cartesian-product syn-catses)
	      )))
      (case (length syn-sems)
        (0 nil)
	(1 (first syn-sems))
	(otherwise `(or ,@syn-sems))
	))
    ))

(defun convert-input-text (text)
  (destructuring-bind (x &key root lfs syntax words) text
      (declare (ignore x))
    `(
      (text ,(words-to-str words))
      (lf-root ,root)
      (lf-terms ,@(wnsenses-to-syms lfs))
      (syntax-tree ,syntax)
      )))

(defun convert-example (ex)
  `(example ,@(convert-input-text ex)))

(defun convert-definition (def)
  `(definition ,@(convert-input-text def)))

(defun convert-concept (pos msg)
  (destructuring-bind (dc concept-name roles sense-keys definitions examples) msg
      (declare (ignore dc sense-keys))
    `(concept ,(repkg concept-name :wn) ; FIXME should refer to synset, not sense
      (sem-frame ,@(mapcar #'role-to-role-restr-map (remove nil roles)))
      ,(roles-to-syn-sem pos roles)
      ,@(mapcar #'convert-definition definitions)
      ,@(mapcar #'convert-example examples)
      )))

(defun run (&key pos)
  (format t "~s~%~%" '(provenance gloss))
  (loop for msg = (read *standard-input* nil) while msg
        when (eq 'define-concept (car msg)) do
	(handler-case (convert-concept pos msg)
	  (error (e)
	    (format t "; Error converting concept ~s~%; ~a" (second msg) e))
	  (:no-error (converted)
	    (format t "~s" converted))
	  )
	  (format t "~%~%")
	))

