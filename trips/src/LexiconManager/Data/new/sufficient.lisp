;;;;
;;;; W::SUFFICIENT
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::SUFFICIENT
   (wordfeats (W::morph (:FORMS (-LY))))
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date 20090731 :comments nil :wn ("sufficient%3:00:00"))
     (EXAMPLE "that's sufficient [for him]")
     (LF-PARENT ONT::ADEQUATE)
     (TEMPL central-adj-optional-xp-TEMPL (XP (% W::PP (W::Ptype W::for))))
     )
    )
   )
))

