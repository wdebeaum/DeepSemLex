;;;;
;;;; W::remind
;;;;

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
	  (W::remind
	   (SENSES
	    ((LF-PARENT ONT::REPEAT)
	     (example "remind me [ of the rule ]")
	     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL (xp (% w::PP (w::ptype w::of))))
	     (meta-data :origin lam :entry-date 20050425 :change-date nil :comments lam-initial)
	     )
	    ((LF-PARENT ONT::REPEAT)
	     (example "remind him about the date")
	     (TEMPL AGENT-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL (xp (% w::PP (w::ptype w::about))))
	     (meta-data :origin lam :entry-date 20050425 :change-date nil :comments lam-initial)
	     )	    
	    ((EXAMPLE "remind him that the meeting is tomorrow")
	     (LF-PARENT ONT::REPEAT)
	     (meta-data :origin lam :entry-date 20050425 :change-date nil :comments lam-initial)
	     (TEMPL AGENT-ADDRESSEE-THEME-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
	     )
	    (;;(LF-PARENT ONT::COMMAND)
	     (lf-parent ont::remind) ;;  20120524 GUM change new parent
	     (example "remind him to do it")
	     (meta-data :origin lam :entry-date 20050425 :change-date nil :comments lam-initial)
	     (TEMPL AGENT-addressee-theme-OBJCONTROL-REQ-TEMPL)
	     )
	    ))
))

