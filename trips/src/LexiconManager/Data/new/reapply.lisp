;;;;
;;;; W::reapply
;;;;

(define-words :pos W::V 
  :words (
	  (W::reapply
	   (SENSES
	    ((LF-PARENT ONT::USE)
	     (example "reapply the rule [to this example]")
	     (SEM (F::ASPECT F::DYNAMIC) (F::cause f::agentive))
	     (meta-data :origin task-learning :entry-date 20050831 :change-date nil :comments nil)
	     (TEMPL AGENT-AFFECTED-FORMAL-XP-OPTIONAL-A-TEMPL  (xp (% W::PP (w::ptype w::to))))
	     )
	    ))
))

