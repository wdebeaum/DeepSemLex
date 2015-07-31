;;;;
;;;; W::fold
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::fold
   (SENSES
    ((LF-PARENT ONT::fold)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin task-learning :entry-date 20050926 :change-date nil :comments nil :vn ("bend-45.2") :wn ("fold%2:30:10" "fold%2:35:00" "fold%2:30:00"))
     (example "fold the paper in half")
     )
    )
   )
))

(define-words :pos W::V 
  :templ agent-theme-xp-templ
 :words (
((w::fold (w::in))
 (senses
  ((meta-data :origin foodkb :entry-date 20050811 :change-date nil :comments nil)
   (LF-PARENT ONT::combine-objects)
   (example "fold in the remaining ingredients")
   (SEM (F::Cause F::Agentive) (F::Aspect F::bounded) (F::Time-span F::atomic))
   )
  )
 )
))
