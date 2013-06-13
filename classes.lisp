;;;; class and generic method declarations (must be loaded before any other code using them, to avoid breaking some Lisps)

(in-package :dsl)

(locally (declare (optimize safety))

(defclass-simple input-text ()
  "A chunk of text (usually a sentence) to be used as input to the parser."
  (string text "" "the text itself")
  (list source nil "a list describing where this text came from")
  (list lattice nil "a list of TextTagger-like messages to the parser")
  )

(defclass-simple concept ()
  "An abstract top-level concept class. Different aspects of lexical concepts are their own subclasses, which may be combined together to form full concepts."
  (symbol name (intern (symbol-name (gensym "C")) :lexicon-data) "the name of the concept")
  ((list-of symbol) aliases nil "alternative names for the concept")
  ((list-of input-text) definitions nil)
  ((list-of relation) out nil "the list of relations where this is the source")
  ((list-of relation) in nil "the list of relations where this is the target")
  )

(defgeneric merge-concepts (dst src) (:documentation
  "Add the information in concept src to concept dst, destructively. Signal an
   error if the two conflict."))

(defclass-simple relation ()
  "A labeled, directed link between two concepts."
  ((maybe-disj concept) source)
  (symbol label) ; NOTE: may be :inherits-from, :maps-to/from, :nominalization, ...
  ((maybe-disj concept) target)
  )

(defgeneric add-relation (source label target))
(defmethod add-relation ((source concept) (label symbol) (target concept))
  (let ((r (make-relation :source source :label label :target target)))
    (push r (out source))
    (push r (in target))
    ))

(defclass-simple role-restr-map ()
  "A semantic role and a restriction of the concepts that may play that role."
  ((list-of sem-role) roles nil "the semantic role name(s)")
  ((disj-conj concept) restriction nil "the concept restricting what may play that role")
  (boolean optional t "t if a player of this role is not required to be present")
  )

(defmethod merge-concepts ((dst role-restr-map) (src role-restr-map))
  ; TODO intersect (restriction dst/src)?
  )

(defclass-simple sem-frame (concept)
  "A semantic frame."
  ((list-of role-restr-map) maps)
  )

(defclass-simple semantics (concept)
  ""
  ((maybe-disj sem-frame) sem-frame)
  (sem-feats features nil "legacy semantic features")
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
  (syn-arg syn-arg nil "the syntactic argument name")
  (syn-cat syn-cat nil "the POS/phrase tag of the argument")
  ((maybe sem-role) sem-role nil "the semantic role played by the argument")
  )

(defclass-simple syn-sem (concept)
  ""
  ((list-of syn-sem-map) maps)
  )

(defclass-simple syntax (concept)
  "A syntactic frame and its features."
  ((maybe-disj syn-sem) syn-sem)
  (syn-feats features nil "a simple feature/value map used by the grammar")
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
  ((list-of symbol) remaining-words "All the words except the first and the particle.")
  ((maybe symbol) particle)
  )

(defclass-simple morph-map ()
  ""
  (syn-feats features)
  (word morphed)
  )

(defclass-simple morph ()
  ""
  (pos pos)
  ((list-of morph-map) maps)
  )

(defclass-simple sense (syntax semantics)
  "A concrete, bottom-level concept class, possibly associated with a morph."
  (morph morph)
  ((list-of input-text)	examples)
  )

(defclass-simple lexicon-and-ontology ()
  "A database of words, concepts, and relationships among them."
  ((hash :to concept) concepts (make-hash-table :test #'eq))
  ((hash :from (list-of symbol) :to (list-of sense))
    senses (make-hash-table :test #'equalp))
  )

) ; end (optimize safety)
