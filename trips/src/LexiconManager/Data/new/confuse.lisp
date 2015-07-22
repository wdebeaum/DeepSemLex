;;;;
;;;; W::confuse
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::confuse
   (SENSES
    ((meta-data :origin "verbnet-2.0-corrected" :entry-date 20060315 :change-date 20090512 :comments nil :vn ("amuse-31.1") :wn ("confuse%2:31:03" "confuse%2:37:00"))
     (LF-PARENT ONT::evoke-confusion)
     (TEMPL agent-affected-xp-templ) 
     (PREFERENCE 0.96)
     )
    ((meta-data :origin "verbnet-1.5-corrected" :entry-date 20051219 :change-date nil :comments nil :vn ("amalgamate-22.2-2"))
     (LF-PARENT ONT::confuse)
     (TEMPL agent-neutral-theme-templ (xp2 (% w::pp (w::ptype w::with)))) ; like associate but changed sense to ont::confuse & agent role to cognizer
     (PREFERENCE 0.96)
     )
    ((EXAMPLE "He confused the issues ")
     (LF-PARENT ONT::CONFUSE)
     (TEMPL agent-neutral-TEMPL)
     )
    )
   )
))

