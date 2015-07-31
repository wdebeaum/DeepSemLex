;;;;
;;;; W::unacceptably
;;;;

(define-words :pos W::adv :templ PRED-VP-TEMPL 
 :words (
	(W::unacceptably
	  (SENSES
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL ADJ-OPERATOR-TEMPL)	    
	    (example "his ankles are unacceptably weak" "he is unacceptably lazy")
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   ((LF-PARENT ONT::acceptability-val)
	    (TEMPL PRED-VP-TEMPL)	    
	    (example "he performed unacceptably on the exam")
	    (SEM (f::gradability +) (f::orientation f::neg) (f::intensity f::med))
	    (meta-data :origin cardiac :entry-date 20080613 :change-date nil :comments nil :wn nil)
	    )
	   )
	  )
))
