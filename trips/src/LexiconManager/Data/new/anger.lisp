;;;;
;;;; W::anger
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::anger
   (wordfeats (W::morph (:forms (-vb) :past W::angered :ing W::angering)))
   (SENSES
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date 20090511 :comments nil :vn ("amuse-31.1") :wn ("anger%2:37:00" "anger%2:37:01"))
     (LF-PARENT ONT::evoke-anger)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
   (W::anger
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (LF-PARENT ONT::affect-experiencer)
     (TEMPL agent-affected-xp-templ) ; like annoy,bother,concern,hurt
     (syntax (w::resultative +))
     )
    )
   )
))

