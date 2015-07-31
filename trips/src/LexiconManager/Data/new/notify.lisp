;;;;
;;;; W::notify
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::notify
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (EXAMPLE "notify him that the plan changed")
     (LF-PARENT ONT::inform)
     (TEMPL AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((meta-data :origin trips :entry-date 20060414 :change-date 20090506 :comments nil :vn ("advise-37.9"))
     (EXAMPLE "notify him about the change")
     (LF-PARENT ONT::inform)
     (TEMPL AGENT-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL)
     )
    )
   )
))
