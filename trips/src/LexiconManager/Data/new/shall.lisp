;;;;
;;;; W::shall
;;;;

(define-words :pos W::v :boost-word t :TEMPL AGENT-FORMAL-XP-TEMPL
 :tags (:base500)
 :words (
  (W::shall
   (SENSES
    ;;;; I shall drive a truck
    ((LF-PARENT ONT::FUTURE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::shall)
     (TEMPL AUX-FUTURE-TEMPL)
     (SYNTAX (W::VFORM W::FUT))
     )
    ((LF-PARENT ONT::FUTURE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::shall)
     (TEMPL MODAL-AUX-NOCOMP-TEMPL)
     (SYNTAX (W::VFORM W::FUT) (W::changesem -))
     )
    )
   )
))

