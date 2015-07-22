;;;;
;;;; W::spot
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::spot
   (SENSES
    ((meta-data :origin medadvisor :entry-date 20060803 :change-date 20071024 :comments nil)
     (LF-PARENT ONT::loc-as-area)
     )
    )
   )
))

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::spot
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("sight-30.2") :wn ("spot%2:30:00" "spot%2:39:00"))
     (LF-PARENT ONT::active-perception)
     (TEMPL agent-neutral-templ) ; like observe,view,watch
     )
    )
   )
))

