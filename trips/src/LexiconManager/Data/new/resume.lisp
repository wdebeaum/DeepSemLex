;;;;
;;;; W::resume
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::resume
   (SENSES
    ((lf-parent ont::resume-action)
     (SEM (F::Cause F::Agentive) (F::Locative F::Located) (F::Time-span F::extended))
     (example "resume searching")
     (TEMPL AGENT-ACTION-SUBJCONTROL-TEMPL (xp (% w::VP (w::vform w::ing))))
     (meta-data :origin plow :entry-date 20050401 :change-date 20091008 :comments nil)
     )
    ((lf-parent ont::resume-action)
     (SEM (F::Cause F::Agentive) (F::Locative F::Located) (F::Time-span F::extended))
     (example "resume the search")
     (TEMPL AGENT-ACTION-OPTIONAL-TEMPL)
     (meta-data :origin plow :entry-date 20050401 :change-date 20091008 :comments nil)
     )
    ))
))
