;;;;
;;;; W::append
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::append
   (SENSES
    ((LF-PARENT ONT::attach)
     (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-AFFECTED-AFFECTED1-XP-OPTIONAL-TEMPL (xp (% W::PP (W::ptype W::to))))
     (meta-data :origin task-learning :entry-date 20050823 :change-date nil :comments nil)
     (example "append the messages to this message" "append the glossary to the novel")
     )
    )
   )
))

