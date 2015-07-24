(in-package :dsl)

(defvar *db* (make-instance 'lexicon-and-ontology))

(defun get-or-make-concept (name &optional (concept-type 'concept) provenance)
  "Get the named concept if it exists and extend it to the given subtype of
   concept if necessary, or create a new concept of that type with the given
   provenance if it doesn't yet exist."
  ;; TODO add aliases to (concepts *db*) too; merge concepts when alias already
  ;; exists as an independent concept... also need to update relations and
  ;; their opposite sides
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
    ((not (slot-exists-p whole-instance part-type))
      ;; TODO recurse on concept parts of whole-instance?
      (error "Not sure what part of ~s is ~s" (type-of whole-instance) part-type))
    ((not (slot-boundp whole-instance part-type))
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

(defun add-references-from-concept-formula (f)
  "Given a concept formula (i.e. a (disj-conj concept)), add each cons cell whose car is a concept to that concept's references."
    (declare (type (disj-conj concept) f))
  (loop for c on (cdr f)
  	do (if (consp (car c))
	     (add-references-from-concept-formula (car c))
	     (pushnew c (references (car c)) :test #'eq)
	     )
	))

(defun add-relation (source label target &optional provenance)
    (declare (type symbol label)
             (type (maybe-disj concept) source target)
             (type (maybe provenance) provenance))
  "Make a relation and add it to its source and target concepts if appropriate."
  ;; FIXME don't add a relation if it's already there
  (let ((r (make-instance 'relation :source source :label label :target target :provenance provenance)))
    (if (consp source)
      (add-references-from-concept-formula source)
      (push r (out source))
      )
    (if (consp target)
      (add-references-from-concept-formula target)
      (push r (in target))
      )
    ))

(defun anonymous-concept-p (x)
  (and (typep x 'concept)
       (eql (symbol-package (name x)) (find-package :ld))
       (char= (elt (symbol-name (name x)) 0) #\C)
       (every #'digit-char-p (subseq (symbol-name (name x)) 1))
       ))

(defvar *dummy-morph-map-syn-feats* (make-instance 'syn-feats :features '((agr |3S|) (vform base))))

(defvar *syn-feats-to-suffix* 
  (mapcar
    (lambda (p)
      (cons (make-instance 'syn-feats :features (first p))
            (second p)
	    ))
    '(
      (((pos V) (form |3S|) (vform pres) (agr |3S|)) "S")
      (((pos V) (form |12S123PBASE|) (vform (w::or base pres)) (agr (w::or |1S| |2S| |1P| |2P| |3P|))) "")
      (((pos V) (form ing) (vform ing)) "ING")
      (((pos V) (form past) (vform past)) "ED")
      (((pos V) (form pastpart) (vform pastpart)) "ED")
      (((pos N) (form sing) (agr |3S|)) "")
      (((pos N) (form plur) (agr |3P|)) "S")
      (((pos ADJ) (form none) (comparative -)) "")
      (((pos ADJ) (form er) (comparative +)) "ER")
      (((pos ADJ) (form est) (comparative superl)) "EST")
      (((pos N) (form nom)) nil) ; hack to let :nom "irregular form" work, even though there's no "regular" nom
      )
    )
  "An assoc list from syn-feats to regular suffix strings."
  )

(defun add-suffix-to-word (w s)
    (declare (type word w) (type string s))
  (let* ((non-particle-words (cons (first-word w) (remaining-words w)))
         (morphed-npw (util::add-suffix non-particle-words s)))
    (make-instance 'word
        :first-word (car morphed-npw)
	:remaining-words (cdr morphed-npw)
	:particle (particle w)
	)))

(defun add-morph-maps-for-word (m w)
    (declare (type morph m) (type word w))
  "Fill in any missing morph-maps in m corresponding to the base form w and
   appropriate inflections for (pos m)."
  (case (pos m)
    ((N V ADJ)
      (loop for feats-suffix in *syn-feats-to-suffix*
	    for feats = (car feats-suffix)
	    for suffix-pos = (second (assoc 'pos (features feats)))
	    for suffix-form = (second (assoc 'form (features feats)))
	    for suffix = (cdr feats-suffix)
	    when (and suffix
	              (eq (pos m) suffix-pos)
	    	      ;; we don't already have a map for this form/pos
	              (not (member
		               `((pos ,suffix-pos) (form ,suffix-form))
			       (maps m)
			       :key (lambda (mm) (features (syn-feats mm)))
			       :test
			         (lambda (f1 f2)
				   ;; f1 unifies with f2, with no remainder for
				   ;; f1
				   (null (first (nth-value 2 (unify-feats f1 f2)))))
			       )))
	      do (push (make-instance 'morph-map
			   :morphed (add-suffix-to-word w suffix)
			   :syn-feats feats)
		       (maps m))
	    ))
    (otherwise
      (push (make-instance 'morph-map
                :morphed w
		:syn-feats
		  (make-instance 'syn-feats
		      :features `((pos ,(pos m)) (form none)))
		)
            (maps m)
	    ))
    ))

(defun add-morphed-sense-to-db (db sense)
    (declare (type lexicon-and-ontology db) (type sense sense))
  "Add sense to (senses db) keyed from each morphed form."
  (unless (maps (morph sense))
    (warn "adding no morph-maps to (senses db) for sense~%~s" sense))
  (dolist (word (mapcar #'morphed (maps (morph sense))))
    ;; index by...
    (let ((keys (list 
		      ;; ... first word
                      (list (first-word word))
		      ;; ... all contiguous words
                      (cons (first-word word) (remaining-words word)))))
      ;; ... all words
      (when (particle word)
	(push `(,(first-word word) ,@(remaining-words word) ,(particle word))
	      keys))
      (setf keys (util::convert-to-package keys :w))
;      (format t "keys for ~s are ~s~%" word keys)
      (dolist (key keys)
	(pushnew sense (gethash key (senses db))))
      )))

