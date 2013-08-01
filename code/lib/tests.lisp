(load "defsys")
(in-package :dsl)
(load "lisp-unit")
(use-package :lisp-unit)

(defmacro assert-type (expected-type value-form &rest extras)
  `(assert-equality #'subtypep (type-of ,value-form) ',expected-type ,@extras))

(defmacro with-clean-db (&body body)
  `(let ((*db* (make-instance 'lexicon-and-ontology))
         *concept-stack*
	 *current-provenance*
	 *current-input-text*
	 *current-word*
	 *current-pos*
	 *current-morph*)
    ,@body))

(define-test anonymous-concept
  (with-clean-db
    (ld::concept
      (assert-true (anonymous-concept-p (current-concept))))
    (assert-equal nil (current-concept))
    ))

(define-test named-concept
  (with-clean-db
    (ld::concept the-name-of-the-concept
      (assert-type concept (current-concept))
      (assert-false (anonymous-concept-p (current-concept)))
      (assert-equal 'the-name-of-the-concept (name (current-concept)))
      (assert-equal (current-concept) (gethash 'the-name-of-the-concept (concepts *db*)))
      )
    (assert-equal nil (current-concept))
    (assert-true (gethash 'the-name-of-the-concept (concepts *db*)))
    ))

(define-test concept-subtypes
  (with-clean-db
    (ld::syn-feats a-syn-feats)
    (assert-type syn-feats (gethash 'a-syn-feats (concepts *db*)))
    (ld::sem-feats a-sem-feats)
    (assert-type sem-feats (gethash 'a-sem-feats (concepts *db*)))
    (ld::syn-sem a-syn-sem)
    (assert-type syn-sem (gethash 'a-syn-sem (concepts *db*)))
    (ld::sem-frame a-sem-frame)
    (assert-type sem-frame (gethash 'a-sem-frame (concepts *db*)))
    (ld::entailments a-entailments)
    (assert-type entailments (gethash 'a-entailments (concepts *db*)))
    (loop for v being the hash-values of (concepts *db*) do
      (assert-type concept v))
    ))

(define-test verbnet-ish
  (with-clean-db
    (ld::provenance ld::VerbNet (ld::version "fake") (ld::filename "some_verb-12.3.xml"))
    (assert-equal "fake" (version *current-provenance*))
    (ld::concept VN::some_verb-12.3
      (ld::aliases VN::_12.3)
      (ld::sem-frame
        (VN::Agent (ld::sem-feats (VN::int_control ld::+)))
	(VN::Patient (ld::sem-feats (VN::concrete ld::+)))
	)
      (ld::concept VN::some_verb-12.3-4
        (ld::aliases VN::_12.3-4)
	(ld::overlap WN::|some_verb%2:00:00::| WN::|some_other_verb%2:00:00::|)
	(w::or ; VN frames
	  (ld::concept
	    (ld::example (ld::text "this is an example of some verb"))
	    (ld::syn-sem
	      (ld::lsubj ld::NP VN::Agent)
	      (ld::lobj ld::NP VN::Patient)
	      )
	    (ld::entailments
	      "some_verb-12.3-4 => some consequent"
	      "some_verb-12.3-4 => some other consequent"
	      )
	    )
	  (ld::concept
	    (ld::syn-sem
	      (ld::lsubj ld::NP VN::Patient)
	      )
	    (ld::entailments
	      "some_verb-12.3-4 => blah"
	      )
	    )
	  )
	)
      (ld::concept VN::some_verb-12.3-5
        (ld::aliases VN::_12.3-5)
	; ...
	)
      )
    (assert-eql 8 (hash-table-count (concepts *db*)))
    (assert-eql 2 (length (eval-path-expression '(VN::some_verb-12.3 <inherit))))
    (assert-equal "some_verb-12.3.xml" (filename (first (provenance (gethash 'VN::some_verb-12.3-4 (concepts *db*))))))
    ; TODO more assertions
    ))

(define-test wordnet-ish
  (with-clean-db
    (ld::provenance ld::WordNet (ld::version "fake") (ld::filename "data.noun"))
    (ld::morph (ld::pos ld::N)
      (ld::concept WN::n00001740
        (ld::definition (text "a fake thing"))
	(ld::> WN::Hyponym WN::n00123456)
	(ld::sense WN::|some_noun%1:02:03::|
	  (ld::alias WN::some_noun.n.1)
	  (ld::word (ld::some ld::noun))
	  (ld::> WN::Derivationally_related_form WN::|nounsome.a.1|)
	  )
	(ld::sense WN::|some_other_noun%1:00:00::|
	  (ld::alias WN::some_other_noun.n.2)
	  (ld::word (ld::some ld::other ld::noun))
	  )
	)
      (ld::concept WN::n00123456
        (ld::definition (text "a specific fake thing"))
	(ld::sense WN::|some_specific_noun%1:00:00::|
	  (ld::alias WN::some_specific_noun.n.1)
	  (ld::word (ld::some ld::specific ld::noun))
	  )
	)
      )
    (assert-eql 9 (hash-table-count (concepts *db*)))
    (assert-equalp '(w::specific w::noun) (eval-path-expression '(WN::some_specific_noun.n.1 morph maps #'first morphed remaining-words)))
    ; TODO more assertions
    ))

(run-tests)

