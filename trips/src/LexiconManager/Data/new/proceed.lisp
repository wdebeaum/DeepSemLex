;;;;
;;;; W::proceed
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::proceed
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("begin-55.1"))
     (LF-PARENT ONT::activity-ongoing)
     (TEMPL agent-action-optional-templ (xp (% w::pp (w::ptype w::with)))) ; like go-on
     (PREFERENCE 0.96)
     )
    #||((LF-PARENT ONT::GO-ON)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-TEMPL)
     (example "the truck proceeds to Delta")
     (PREFERENCE 0.98)    ;;;; I want agent interpretation to show up first
     )||#
    ((LF-PARENT ONT::GO-ON)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     (example "proceed to the exit")
     )
    )
   )
))

