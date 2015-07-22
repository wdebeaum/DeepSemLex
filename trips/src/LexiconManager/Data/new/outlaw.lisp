;;;;
;;;; W::outlaw
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::outlaw
   (SENSES
    ((meta-data :origin "wordnet-3.0" :entry-date 20090528 :change-date nil :comments nil)
     (LF-PARENT ONT::prohibit)
     (templ agent-effect-xp-templ)
     (example "The treaty outlawed torture")
     )
    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     (LF-PARENT ONT::prohibit)
     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing))))
     (example "It restricts him from doing something")
     )
    )
   )
))

