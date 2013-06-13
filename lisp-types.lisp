;;;; Extensions to the Lisp type system

(in-package :dsl)

(defmacro defclass-simple (name superclasses doc-string &body slots)
  "A simpler version of defclass that always makes accessors and initargs, uses
   slot descriptions formatted like (type name &optional initform doc-string),
   and uses a class doc string more like defun/defmacro/defmethod."
  `(defclass ,name ,superclasses
     ,(mapcar
	(lambda (slot)
	  (list (second slot)
		:accessor (second slot)
		:initarg (second slot)
		:type (first slot)
		:initform (third slot)
		:documentation (fourth slot)
		))
	slots)
     (:documentation ,doc-string)
     ))

(defpackage :type-predicates)
(deftype list-of (member-type)
  "Dependent list type. 'list was already taken."
  (if (eq t member-type)
    'list
    (let ((predicate-name (intern (format nil "LIST-OF-~s-P" member-type) :type-predicates)))
      (unless (fboundp predicate-name)
	(eval `(defun ,predicate-name (x) (or (null x) (and (consp x) (typep (car x) ',member-type) (,predicate-name (cdr x)))))))
      `(satisfies ,predicate-name)
      )))

(deftype hash (&optional (from 'symbol) (to t))
  "Dependent hash-table type."
  (if (and (eq t from) (eq t to))
    'hash-table
    (let ((predicate-name (intern (format nil "HASH-FROM-~s-TO-~s-P" from to) :type-predicates)))
      (unless (fboundp predicate-name)
	(eval
	  `(defun ,predicate-name (x)
	    (when (hash-table-p x)
	      (maphash
		(lambda (k v) 
		  (unless (and (typep k ',from) (typep v ',to))
		    (return-from ,predicate-name nil)))
		x)
	      t))))
      `(satisfies ,predicate-name)
      )))

(deftype alist (&key (from 'symbol) (to t))
  "Dependent assoc-list type.
   e.g. (typep '((a 1) (b 2)) '(alist :from symbol :to integer)). Dotted pairs
   are not supported, sorry."
  `(list-of (cons ,from (cons ,to null))))

(deftype disjunction (&optional (member-type t))
  `(cons (eql W::or) (list-of ,member-type)))

(deftype conjunction (&optional (member-type t))
  `(cons (eql W::and) (list-of ,member-type)))

(deftype maybe-disj (&optional (member-type t))
  `(or ,member-type (disjunction ,member-type)))

(deftype disj-conj (&optional (terminal-type t))
  `(or ,terminal-type
       (disjunction (disj-conj ,terminal-type))
       (conjunction (disj-conj ,terminal-type))
       ))

(deftype maybe (just-type)
  `(or null ,just-type))

