(in-package :dsl)

(defun partition (key-fn l &key (test #'eql))
  "Given a key function and a list, return a list of pairs (key-val . items),
   where items are the items in the original list for which the key function
   returned key-val."
  (loop with ret = nil
        for item in l
	for key-val = (funcall key-fn item)
	for pair = (or (assoc key-val ret :test test)
	               (car (push (cons key-val nil) ret)))
	do (push item (cdr pair))
	finally (return ret)))

(defmethod dnf ((c concept))
  (let* ((parents
           (mapcar #'target
	           (remove-if-not
		     (lambda (r)
		       ;; TODO also :subtype-of?... filter on provenance?
		       (eq :inherit (label r)))
		     (out c))))
         (parent-dnfs (mapcar #'dnf parents)))
    (cons 'W::OR
          (mapcar (lambda (x)
			   ;; get all the parts to be conjoined that are of the
			   ;; same type
	            (let* ((conj-by-type
		             (partition #'type-of
			                (apply #'append (mapcar #'cdr x))))
			   ;; merge parts of the same type into a new instance
			   (merged-conj
			     (mapcar (lambda (p)
			               (reduce #'merge-concepts
					       (cdr p)
				               :initial-value
					         (make-instance (car p))
					       ))
				     conj-by-type))
			   )
		      ;; TODO merge parts of different types where one is part
		      ;; of another (use concept-part-of-p)
		      (cons 'W::AND merged-conj)))
	          (cartesian-product (mapcar #'cdr parent-dnfs))))))

;; TODO generalize this to operate on all concepts by pushing details into merge-concepts, and use concept-part-of-p to merge parts into wholes
(defmethods dnf ((this (or sem-frame syn-sem)))
  (let ((this-type (type-of this))
        (inherited-dnf (call-next-method)))
    (cons 'W::OR
        (mapcar
	    (lambda (conj)
	      (cons 'W::AND
		  (mapcar
		      (lambda (term)
		        (if (eql this-type (type-of term))
			  (merge-concepts term this)
			  term))
		      (cdr conj)
		      )))
	    (cdr inherited-dnf)
	    ))))

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

