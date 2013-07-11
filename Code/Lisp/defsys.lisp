
(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up :up "config" "lisp")
		       :name "trips")))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(defpackage :deepsemlex
  (:use :common-lisp :util)
  (:nicknames :dsl)
  (:export
    initialize-lexicon
    get-word-def
    retrieve-from-lex
    initialize-ontology
    subtype
    subtype-in
    )
  )

(defpackage :lexiconmanager
  (:use :dsl)
  (:nicknames :lxm)
  )

(defpackage :ontologymanager
  (:use :dsl)
  (:nicknames :om)
  )

(defpackage :w)
; (defpackage :f)
(defpackage :ont)
(defpackage :lexicon-data
  (:use)
  (:nicknames :ld)
  )

;; FIXME need to handle resource namespaces and versioning better
(defpackage :wn (:nicknames :WordNet :WN-3.0 :WN-2.1))
(defpackage :mwo (:nicknames :Merriam-Webster_Online))
(defpackage :mac) ; this is used in OntoNotes, don't know what it is
(defpackage :answers.com)
(defpackage :vn (:nicknames :VerbNet :VN-3.2))
(defpackage :on (:nicknames :OntoNotes :ON-3.0))
(defpackage :fn (:nicknames :FrameNet))
(defpackage :pb (:nicknames :PropBank))

(in-package :dsl)

(mk:defsystem :dsl
  :package dsl
  :depends-on (:util)
  :components (
    "mop"
    "lisp-types"
    "symbol-types"
    "generics"
    "classes"
    "load"
    "print"
    "db"
    )
  )

(mk:load-system :dsl)

