;;;; class declarations (must be loaded before any other code using them, to avoid breaking some Lisps)

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

(defclass-simple relation ()
  "A labeled, directed link between two concepts."
  ((maybe-disj concept) source)
  (symbol label) ; NOTE: may be :inherit, :overlap, :subtype-of, :nominalization...
  ((maybe-disj concept) target)
  ((maybe provenance) provenance "" nil)
  )

(defclass-simple role-restr-map ()
  "A semantic role and a restriction of the concepts that may play that role."
  ((list-of sem-role) roles "the semantic role name(s)")
  ((disj-conj concept) restriction "the concept restricting what may play that role")
  (boolean optional "t if a player of this role is not required to be present" t)
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

) ; end (optimize safety)
