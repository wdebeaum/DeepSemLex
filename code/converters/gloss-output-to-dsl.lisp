(defpackage :ont)
(defpackage :w)
(defpackage :f)
(defpackage :lexiconmanager) ; ugh.
(defpackage :wn)

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
  (destructuring-bind (role-name (trips-class wn-class)) role
    `( ,(repkg role-name :ont)
       ,(if wn-class
         `(and ,trips-class ,(strs-to-wn-sk-syms wn-class))
	 trips-class)
       )))

(defun convert-example (ex)
  (destructuring-bind (x &key root lfs syntax words) ex
      (declare (ignore x))
    `(example
      (text ,(words-to-str words))
      (lf-root ,root)
      (lf-terms ,@(wnsenses-to-syms lfs))
      (syntax-tree ,syntax)
      )))

(defun convert-concept (msg)
  (destructuring-bind (dc concept-name class roles lf-root lf-terms sense-keys examples) msg
      (declare (ignore dc class sense-keys))
    `(concept ,(repkg concept-name :wn) ; FIXME should refer to synset, not sense
      (sem-frame ,@(mapcar #'role-to-role-restr-map (remove nil roles)))
      (definition (lf-root ,lf-root) (lf-terms ,@(wnsenses-to-syms lf-terms)))
      ,@(mapcar #'convert-example examples)
      )))

(defun run ()
  (format t "~s~%~%" '(provenance gloss))
  (loop for msg = (read *standard-input* nil) while msg
        when (eq 'define-concept (car msg)) do
	(format t "~s~%~%" (convert-concept msg))
	))

