;;;;
;;;; W::elude
;;;;

(define-words :pos W::V :templ agent-neutral-xp-templ
 :words (
  (W::elude
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("avoid-52") :wn ("elude%2:32:00" "elude%2:38:00"))
     (LF-PARENT ONT::avoiding)
 ; like circumvent,evade,avoid
     )
    
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("avoid-52") :wn ("elude%2:32:00" "elude%2:38:00"))
     (LF-PARENT ONT::avoiding)
     (TEMPL AGENT-FORMAL-SUBJCONTROL-TEMPL (xp (% w::vp (w::vform w::ing)))) ; like avoid
     )
    )
   )
))

