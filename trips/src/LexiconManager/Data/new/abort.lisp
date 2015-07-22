;;;;
;;;; W::abort
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 (W::abort
   (SENSES
    ((LF-PARENT ONT::CANCEL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-affected-XP-TEMPL)
     (example "abort the plan")
     (meta-data :origin lou2 :entry-date 20061121 :change-date nil :comments nil)
     )
    ((LF-PARENT ONT::CANCEL)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL AGENT-TEMPL)
     (example "abort")
     (meta-data :origin plot :entry-date 20080604 :change-date nil :comments nil)
     )
    )
   )
))

