(in-package :dsl)

(defvar *db* (make-instance 'lexicon-and-ontology))

(defun get-or-make-concept (name &optional (concept-type 'concept) provenance)
  "Get the named concept if it exists and extend it to the given subtype of
   concept if necessary, or create a new concept of that type with the given
   provenance if it doesn't yet exist."
  (let ((c (gethash name (concepts *db*))))
    (cond
      ((null c)
        ;; c doesn't exist yet, make it
	(setf (gethash name (concepts *db*))
	      (make-instance concept-type
	                     :name name
			     :provenance (when provenance (list provenance))
			     )))
      ((subtypep (type-of c) concept-type) ; or eq
        ;; c already has type concept-type, just return it
        c)
      ((subtypep concept-type (type-of c))
        ;; c was previously declared as having a supertype of concept-type
	;; add slots to make it have type concept-type specifically
	(change-class c concept-type))
      )))

(defun concept-part-of-p (part whole)
  "Given two concept types part and whole, return true iff part is part of
   whole (or they are eq)."
  (or (eq part whole)
      (eq 'concept whole) ; anything can be part of a concept by inheritance
      (case whole
	(sense t)
	(semantics
	  (case part
	    ((sem-frame sem-feats entailments) t)
	    (otherwise nil)
	    ))
	(syntax
	  (case part
            ((syn-sem syn-feats) t)
	    (otherwise nil)
	    ))
	(otherwise nil)
	)
      ))

(defun get-or-make-part-of (part-type whole-instance)
  "Get the part of whole-instance that is of type part-type, creating it if it
   doesn't yet exist."
  (cond
    ((eq 'concept (type-of whole-instance))
      (let ((part-instance (make-instance part-type)))
        (add-relation whole-instance :inherit part-instance)
	part-instance))
    ((not (slot-exists-p part-type whole-instance))
      ;; TODO recurse on concept parts of whole-instance?
      (error "Not sure what part of ~s is ~s" (type-of whole-instance) part-type))
    ((not (slot-boundp part-type whole-instance))
      (setf (slot-value whole-instance part-type) (make-instance part-type)))
    ((typep (slot-value whole-instance part-type) part-type)
      (slot-value whole-instance part-type))
    ((typep (slot-value whole-instance part-type) `(disjunction ,part-type))
      (let ((part-instance (make-instance part-type)))
        (push part-instance (cdr (slot-value whole-instance part-type)))
	part-instance))
    (t
      (error "Slot ~s of ~s is already bound to something not of type (maybe-disj ~s): ~s" part-type (type-of whole-instance) part-type (slot-value whole-instance part-type)))
    ))

(defun add-relation (source label target &optional provenance)
    (declare (type symbol label)
             (type (maybe-disj concept) source target)
             (type (maybe provenance) provenance))
  "Make a relation and add it to its source and target concepts if appropriate."
  (let ((r (make-instance 'relation :source source :label label :target target :provenance provenance)))
    (when (typep source 'concept) (push r (out source)))
    (when (typep target 'concept) (push r (in target)))
    ))

(defun anonymous-concept-p (x)
  (and (typep x 'concept)
       (eql (symbol-package (name x)) (find-package :ld))
       (char= (elt (symbol-name (name x)) 0) #\C)
       (every #'digit-char-p (subseq (symbol-name (name x)) 1))
       ))

;;; FIXME merge-concepts seems old and busted, need a better way to do this

(defmethod merge-concepts ((dst role-restr-map) (src role-restr-map))
  ; TODO intersect (restriction dst/src)?
  )

(defmethod merge-concepts ((dst list) (src list))
  "Since a list/feats may be nil, this returns the new feature list instead of modifying dst"
  ;; Unfortunately we can't say this in the parameter list because feats is a
  ;; deftype, not a class.
  (declare (type feats dst src))
  (dolist (feat src)
    (let ((existing (assoc (car feat) dst)))
      (if existing
	(let* ((dst-val (if (consp (second existing)) (second existing) (list (second existing))))
	       (src-val (if (consp (second existing)) (second existing) (list (second existing))))
	       (int-val (intersection dst-val src-val)))
	  (when (null int-val)
	    (error "can't unify ~s with ~s" existing feat))
	  (setf (second existing) int-val)
	  )
	; feat is new in dst
	(push feat dst)
	)))
  dst)

(defmethod merge-concepts ((dst semantics) (src semantics))
  (dolist (role (roles src))
    (let ((existing (find (name role) (roles dst) :key #'name)))
      (if existing
	(merge-concepts existing role)
	(push role (roles dst))
	)))
  (setf (sem dst) (merge-concepts (sem dst) (sem src)))
  (when (next-method-p) (call-next-method))
  )

(defmethod merge-concepts ((dst syntax) (src syntax))
  ;; TODO redo this to take into account disjunctions
  (dolist (arg (arguments src))
    (let ((existing (find (syntactic-argument arg) (arguments dst) :key #'syntactic-argument)))
      (cond
	((not existing)
	  (push arg (arguments dst)))
	((equalp arg existing)
	  nil)
	(t
	  (error "tried to add syntactic argument ~s previously defined as ~s" arg existing))
	)))
  (setf (features dst) (merge-concepts (features dst) (features src)))
  (when (next-method-p) (call-next-method))
  )
