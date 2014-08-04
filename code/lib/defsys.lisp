
(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up :up :up "config" "lisp")
		       :name "trips")))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (fboundp 'util::add-suffix)
  (load #!TRIPS"src;util;add_suffix.polyglot"))

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

(defpackage :w (:use))
(defpackage :f (:use)) ; should go away at some point...
;(defpackage :ont) ; see resources.lisp
(defpackage :lexicon-data
  (:use)
  (:nicknames :ld)
  )

(in-package :dsl)

(mk:defsystem :dsl
  :package dsl
  :depends-on (:util)
  :components (
    "mop"
    "lisp-types"
    "unify"
    "resources"
    "symbol-types"
    "generics"
    "classes"
    "load"
    "load-old"
    "print"
    "make-db"
    "query"
    )
  )

(mk:load-system :dsl)

