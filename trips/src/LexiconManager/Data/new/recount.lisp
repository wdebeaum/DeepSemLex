;;;;
;;;; W::recount
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::recount
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("say-37.7") :wn ("recount%2:32:00"))
     ;;(LF-PARENT ONT::talk)
     (lf-parent ont::mention-claim)
     (TEMPL agent-affected-iobj-theme-templ) ; like say
     )
    )
   )
))

