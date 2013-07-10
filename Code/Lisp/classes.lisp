;;;; class and generic method declarations (must be loaded before any other code using them, to avoid breaking some Lisps)

(in-package :dsl)

(locally (declare (optimize safety))

(defclass-simple provenance ()
  "Where a concept, relation, or input-text came from."
  (symbol name "the name of the annotator or resource (not including version)")
  ((maybe string) version "the version of the resource" nil)
  ((maybe string) filename "the name of the specific file within the resource (should be nil if there is only one file)" nil)
  ((maybe integer) record-number "the index of the record within the file (byte number, line number, sentence number, etc.; resource-specific)" nil)
  )

(defclass-simple input-text ()
  "A chunk of text (usually a sentence) to be used as input to the parser."
  (string text "the text itself")
  ((maybe provenance) provenance "" nil)
  (list lattice "a list of TextTagger-like messages to the parser" nil)
  )

(defclass-simple concept ()
  "An abstract top-level concept class. Different aspects of lexical concepts are their own subclasses, which may be combined together to form full concepts."
  (symbol name "the name of the concept" (intern (symbol-name (gensym "C")) :lexicon-data))
  ((list-of symbol) aliases "alternative names for the concept" nil)
  ((list-of input-text) definitions "" nil)
  ((list-of input-text)	examples "" nil)
  ((list-of relation) out "the list of relations where this is the source" nil)
  ((list-of relation) in "the list of relations where this is the target" nil)
  ((list-of provenance) provenance "" nil)
  )

(defun anonymous-concept-p (x)
  (and (typep x 'concept)
       (eql (symbol-package (name x)) (find-package :ld))
       (char= (elt (symbol-name (name x)) 0) #\C)
       (every #'digit-char-p (subseq (symbol-name (name x)) 1))
       ))

(defgeneric merge-concepts (dst src) (:documentation
  "Add the information in concept src to concept dst, destructively. Signal an
   error if the two conflict."))

(defclass-simple relation ()
  "A labeled, directed link between two concepts."
  ((maybe-disj concept) source)
  (symbol label) ; NOTE: may be :inherits-from, :maps-to/from, :nominalization, ...
  ((maybe-disj concept) target)
  ((maybe provenance) provenance "" nil)
  )

(defgeneric add-relation (source label target &optional provenance))
(defmethod add-relation ((source concept) (label symbol) (target concept) &optional provenance)
  (let ((r (make-instance 'relation :source source :label label :target target :provenance provenance)))
    (push r (out source))
    (push r (in target))
    ))

(defclass-simple role-restr-map ()
  "A semantic role and a restriction of the concepts that may play that role."
  ((list-of sem-role) roles "the semantic role name(s)")
  ((disj-conj concept) restriction "the concept restricting what may play that role")
  (boolean optional "t if a player of this role is not required to be present" t)
  )

(defmethod merge-concepts ((dst role-restr-map) (src role-restr-map))
  ; TODO intersect (restriction dst/src)?
  )

(defclass-simple sem-frame (concept)
  "A semantic frame."
  ((list-of role-restr-map) maps "" nil)
  )

(defclass-simple sem-feats (concept)
  "Legacy semantic features."
  ((feats sem-feat) features "" nil))

(defclass-simple entailments (concept)
  "A list of terms with variables entailed by a concept."
  ((list-of (cons symbol list)) terms "" nil))

(defclass-simple semantics (concept)
  ""
  ((maybe-disj sem-frame) sem-frame)
  ((maybe-disj sem-feats) sem-feats)
  ((maybe-disj entailments) entailments)
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

(defclass-simple syn-sem-map ()
  "(see slot docs)"
  (syn-arg syn-arg "the syntactic argument name")
  (syn-cat syn-cat "the POS/phrase tag of the argument")
  ((maybe (maybe-disj symbol)) head-word "the head word of the argument (often the preposition when syn-cat is PP)" nil)
  ((maybe sem-role) sem-role "the semantic role played by the argument" nil)
  (boolean optional "t if this argument is not required to be present" t)
  )

(defclass-simple syn-sem (concept)
  ""
  ((list-of syn-sem-map) maps "" nil)
  )

(defclass-simple syn-feats (concept)
  "a simple feature/value map used by the grammar"
  ((feats syn-feat) features "" nil))

(defclass-simple syntax (concept)
  "A syntactic frame and its features."
  ((maybe-disj syn-sem) syn-sem)
  ((maybe-disj syn-feats) syn-feats)
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

(defclass-simple word ()
  "A word or multiword expression."
  (symbol first-word)
  ((list-of symbol) remaining-words "All the words except the first and the particle." nil)
  ((maybe symbol) particle "" nil)
  )

(defclass-simple morph-map ()
  ""
  (syn-feats syn-feats "" nil)
  (word morphed)
  )

(defclass-simple morph ()
  ""
  (pos pos)
  ((list-of morph-map) maps "" nil)
  )

(defclass-simple sense (syntax semantics)
  "A concrete, bottom-level concept class, possibly associated with a morph."
  (morph morph)
  )

(defclass-simple lexicon-and-ontology ()
  "A database of words, concepts, and relationships among them."
  ((hash :to concept) concepts "" (make-hash-table :test #'eq))
  ((hash :from (list-of symbol) :to (list-of sense))
    senses "" (make-hash-table :test #'equalp))
  )

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

) ; end (optimize safety)
