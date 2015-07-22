;;;;
;;;; W::HERE
;;;;

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::HERE
   (SENSES
    ((LF-PARENT ONT::here)
     (SYNTAX (W::IMPRO-CLASS ONT::relative-location)
     ))
    #||((LF-PARENT ONT::TO-LOC)
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     (example "move it to here")
     (preference .97) ;; prefer spatial-loc sense for be
     )||#
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::HERE
   (SENSES
    ((LF-PARENT ONT::PLACE)
     (PREFERENCE 0.98)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ;; added for CAET
   ((W::here w::goes)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
     ))
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::here w::we w::go)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    ))
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::here w::you w::go)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    )
   )
))

