;;;;
;;;; W::deter
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::deter
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("forbid-67"))
     (LF-PARENT ONT::hindering)
     (TEMPL agent-affected-xp-templ) 
 ; like block
     )
      ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
       (LF-PARENT ONT::hindering)
     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing))))
     (example "It deters him from doing something")
     )
    )
   )
))

