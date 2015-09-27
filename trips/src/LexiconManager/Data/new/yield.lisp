;;;;
;;;; W::yield
;;;;

(define-words :pos W::V :templ agent-affected-xp-templ
 :words (
  (W::yield
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("future_having-13.3") :wn ("yield%2:40:01"))
     (LF-PARENT ONT::surrender)
      (templ agent-affected-goal-optional-templ (xp (% W::pp (W::ptype W::to))))
     ;;(TEMPL agent-affected-goal-optional-templ) ; like grant,offer
     )
    )
   )
))

