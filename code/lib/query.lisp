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

(defun hash-table-keys (h)
  (loop for k being the hash-keys of h collect k))

;;; helpers for eval-path-expression

(defmacro add-seed-to-output (seed output)
  `(if (hash-table-p output)
     (pushnew (list ,seed) (gethash ,seed ,output) :test #'equalp)
     (pushnew ,seed ,output :test #'eq)
     ))

(defmacro add-step-to-output (input source label target output)
  `(if (hash-table-p ,output)
     (let ((step-path (list ,target ,label ,source)))
       (dolist (source-path (gethash ,source ,input))
	 (pushnew (append step-path source-path) (gethash ,target ,output)
		  :test #'equalp)))
     (pushnew ,target ,output :test #'eq)
     ))

(defun output-empty-p (output)
  (or (and (hash-table-p output) (> (hash-table-count output) 0))
      (listp output)))

;; TODO need to be able to specify predicates on relations as well as concepts,
;; or at least specify provenance somehow
(defun eval-path-expression (expr &optional input (db *db*))
  "Given a path expression and a list of input concepts (or values in general,
   but this is mostly intended for concepts), return the list of output
   concepts reachable from the input concepts via paths matching the
   expression.

   If the input is a hash table instead of a list, its keys are taken to be the
   input concepts, and its values are lists of paths back from those concepts
   to the original input for the whole expression. Paths alternate between
   concepts and basic steps. Then this function returns a new hash table in the
   same format for the output concepts.
   
   Path expressions are similar to those in WordNetPath, but are S-expressions
   instead of strings, and operate mainly on slots and relations instead of
   WordNet pointers (though WN pointers can be relations too).
   
   Path expression components:
     Seed:
       resource::concept-name
     Basic steps:
       >relation-label
       <reverse-relation-label
       slot-name
       #'function-name
       (lambda (concept) lisp code...) - call the function and use the returned
	 list of concepts as the output. If the return value isn't a list, wrap
	 it in a list.
     Sequencing and repetition:
       (1 exprs...) - follow the exprs in sequence exactly once
       (? exprs...) - zero or one times
       (+ exprs...) - one or more times
       (* exprs...) - any number of times
       (count m n exprs...) - between m and n times, inclusive
       (count m nil exprs...) - at least m times
     Predicates:
       (when exprs...) - keep those input concepts for which anything is
         reachable via the sequence of exprs
       (unless exprs...) - keep those input concepts for which nothing is
         reachable via the sequence of exprs
     Set operations:
       (& exprs...) - the output is the intersection of the outputs of the
         exprs evaluated on the same input
       (/ exprs...) - union
       (- exprs...) - difference (the first expr is positive, the rest are
         negative)
   "
  (let ((output (when (hash-table-p input) (make-hash-table :test #'eq))))
    (cond
         ;; basic steps
     ((symbolp expr)
       (cond
	 ((not (eq (symbol-package expr) (find-package :dsl))) ; seed
	   (let ((val (gethash expr (concepts db))))
	     (when val
	       (add-seed-to-output val output)
	       )))
	 ((char= #\> (elt (symbol-name expr) 0)) ; relation
	   (let ((label (intern (subseq (symbol-name expr) 1) :keyword))
		 (input-concepts (if (hash-table-p input)
				   (hash-table-keys input)
				   input)))
	     (dolist (i input-concepts)
	       (dolist (r (out i)) ; <--
		 (when (eq label (label r)) ;        vvvvvv
		   (add-step-to-output input i expr (target r) output))))))
	 ((char= #\< (elt (symbol-name expr) 0)) ; reverse relation
	   (let ((label (intern (subseq (symbol-name expr) 1) :keyword))
		 (input-concepts (if (hash-table-p input)
				   (hash-table-keys input)
				   input)))
	     (dolist (i input-concepts)
	       (dolist (r (in i)) ; <--
		 (when (eq label (label r)) ;        vvvvvv
		   (add-step-to-output input i expr (source r) output))))))
	 (t ; slot
	   (dolist (i input)
	     (when (and (slot-exists-p i expr) (slot-boundp i expr))
	       (let ((o (slot-value i expr)))
		 (add-step-to-output input i expr o output)
		 ))))
	 ))
     ((functionp expr)
       (let ((input-concepts (if (hash-table-p input)
			       (hash-table-keys input)
			       input)))
	 (dolist (i input-concepts)
	   (let ((ret (funcall expr i)))
	     (dolist (o (if (listp ret) ret (list ret)))
	       (add-step-to-output input i expr o output))))))
     ((listp expr)
       (ecase (car expr)
         (lambda
	   ;; just like functionp, except we have to eval it first
	   ;; the step label is still expr, though
	   (let ((fn (eval expr))
		 (input-concepts (if (hash-table-p input)
				   (hash-table-keys input)
				   input)))
	     (dolist (i input-concepts)
	       (let ((ret (funcall fn i)))
		 (dolist (o (if (listp ret) ret (list ret)))
		   (add-step-to-output input i expr o output))))))
	 ;; sequencing and repetition
	 (1
	   (loop with prev = input
		 for empty-p = (output-empty-p prev)
		 for subexpr in (cdr expr)
		 while empty-p
		 do (setf prev (eval-path-expression subexpr prev db))
		 finally (when empty-p (setf output prev))
		 ))
	 (? (eval-path-expression `(count 0 1 ,@(cdr expr)) input db))
	 (+ (eval-path-expression `(count 1 nil ,@(cdr expr)) input db))
	 (* (eval-path-expression `(count 0 nil ,@(cdr expr)) input db))
	 (count
	   (destructuring-bind (_ min-count max-count &rest subexprs) expr
	     (let ((prev input) next (once-expr `(1 ,@subexprs)))
	       (loop for c from 1 upto min-count
		     do
		       (setf prev next)
		       (setf next
			     (eval-path-expression once-expr prev db))
		       (when (output-empty-p next)
			 (loop-finish))
		     )
	       (unless (output-empty-p next)
		 (if max-count
		   (loop for c from min-count upto max-count
			 do
			   (setf prev next)
			   (setf next
				 (eval-path-expression once-expr prev db))
			   (when (output-empty-p next)
			     (setf next prev)
			     (loop-finish))
			 finally (setf output next)
			 )
		   (loop for c from min-count
			 do
			   (setf prev next)
			   (setf next
				 (eval-path-expression once-expr prev db))
			   (when (output-empty-p next)
			     (setf next prev)
			     (loop-finish))
			 finally (setf output next)
			 )
		   ))
	       )))
	 ;; predicates
	 (when
	   (unless (output-empty-p (eval-path-expression
				       `(1 ,@(cdr expr)) input db))
	     (setf output input)))
	 (unless
	   (when (output-empty-p (eval-path-expression
				     `(1 ,@(cdr expr)) input db))
	     (setf output input)))
	 ;; set operations
	 (& ; intersection
	   ;; TODO might be possible to avoid evaluating later subexpressions
	   ;; if we already know the whole expression yields no concepts
	   (let ((se-outputs
		   (mapcar
		       (lambda (subexpr)
			 (eval-path-expression subexpr input db))
		       (cdr expr))))
	     (if (hash-table-p output)
	       (setf output 
		 (reduce
		     (lambda (output-so-far next-output)
		       (maphash
			   (lambda (concept paths)
			       (declare (ignore paths))
			     (unless (gethash concept next-output)
			       (remhash concept output-so-far)))
			   output-so-far)
		       output-so-far)
		     se-outputs))
	       ; else, lists
	       (setf output
		     (reduce
			 (lambda (a b) (intersection a b :test #'eq))
			 se-outputs
			 ))
	       )))
	 (/ ; union
	   (let ((se-outputs
		   (mapcar
		       (lambda (subexpr)
			 (eval-path-expression subexpr input db))
		       (cdr expr))))
	     (if (hash-table-p output)
	       (setf output
		 (reduce
		     (lambda (output-so-far next-output)
		       (maphash
			   (lambda (concept paths)
			     (setf (gethash concept output-so-far)
				   (union (gethash concept output-so-far)
					  (gethash concept next-output)
					  :test #'equalp)
				   ))
			   output-so-far)
		       output-so-far)
		     se-outputs))
	       ; else, lists
	       (setf output
		     (reduce
		         (lambda (a b) (union a b :test #'eq))
			 se-outputs
			 ))
	       )))
	 (- ; difference
	   ;; TODO same issue as intersection
	   (let ((se-outputs
		   (mapcar
		       (lambda (subexpr)
			 (eval-path-expression subexpr input db))
		       (cdr expr))))
	     (if (hash-table-p output)
	       (setf output
		 (reduce
		     (lambda (output-so-far next-output)
		       (maphash
			   (lambda (concept paths)
			       (declare (ignore paths))
			     (when (gethash concept next-output)
			       (remhash concept output-so-far)))
			   output-so-far)
		       output-so-far)
		     (cdr se-outputs)
		     :initial-value (car se-outputs)
		     ))
	       ; else, lists
	       (setf output
		     (reduce
		         (lambda (a b) (set-difference a b :test #'eq))
			 (cdr se-outputs)
			 :initial-value (car se-outputs)
			 ))
	       )))
	 ))
     (t
       (error "expected symbol, function, or list as path expression, but got: ~s" expr))
     )
   output))


#| probably still too general
(defun traversal-tree (start visit-fn &optional (traversed (make-hash-table :test #'eq)))
  "Do a depth-first traversal of a network of class/structure instance, calling
   visit-fn on each one and returning a tree representing the traversal.
   start - the instance to start at
   visit-fn - a function taking an instance and returning a list of slots to
   traverse (possibly empty)
   traversed - a hash table mapping already-traversed instances to their tree
   values
   "
  (multiple-value-bind (old-tree present-p) (gethash start traversed)
    (when present-p
      (return-from traversal-tree old-tree)))
  (let ((tree (list start)))
    (setf (gethash start traversed) tree)
    (let ((child-slots (funcall visit-fn start)))
      (setf (cdr tree)
            (mapcar
	        (lambda (slot)
		  (traversal-tree (slot-value start slot) visit-fn traversed))
		child-slots
		)
	    ))
    tree))
|#

#| argh, this won't work
(defun traversal-tree (start visit-fn &optional (traversed (make-hash-table :test #'eq)))
  "Traverse a network of class/structure instances calling visit-fn on each one
   and building a tree representing the traversal to return. visit-fn should
   take an instance and return two or three values: a list of the names of
   slots to traverse, a boolean indicating whether this instance should have
   its own node in the final tree (t) or just have its children included among
   its parent's (nil), and optionally the car of the node in the final tree.
   Each instance will be visited exactly once; the result tree for an instance
   reachable in more than one way will be shared, by storing the return values
   of this function in the traversed hash. This function returns two values: a
   tree or a list of child trees, and a boolean indicating which it is (t=tree,
   nil=list of children) It is possible to create circular data structures with
   this function, so setting *print-circle* to t is recommended if calling this
   from the REPL."
  (multiple-value-bind (old-tree present-p) (gethash start traversed)
    (when present-p
      (return-from traversal-tree (values-list old-tree))))
  (let ((placeholder (cons nil nil)))
    (setf (gethash start traversed) placeholder)
    (multiple-value-bind (child-slots new-node-p new-node-car)
	(funcall visit-fn start)
      (
  ))))
|#

#| old and busted

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

|#

