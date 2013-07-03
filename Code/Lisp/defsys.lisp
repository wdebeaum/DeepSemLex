
(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
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
(defpackage :wn)
(defpackage :vn)
(defpackage :lexicon-data
  (:use)
  (:nicknames :ld)
  )

(in-package :dsl)

(mk:defsystem :dsl
  :package dsl
  :depends-on (:util)
  :components (
    "lisp-types"
    "symbol-types"
    "classes"
    "data"
    )
  )

(mk:load-system :dsl)

