;;;;
;;;; W::return
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
;   )
  (W::return
   (SENSES
;    ((meta-data :origin plow :entry-date 20060608 :change-date nil :comments pq)
;     (example "put the return date here")
;     (LF-PARENT ONT::event)
;     )
    ((LF-PARENT ONT::letter-symbol)
     (EXAMPLE "join the text from both cells, separated by a carriage return")
     (meta-data :origin task-learning :entry-date 20050919 :change-date nil :wn ("carriage_return%1:22:00") :comments nil)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::return
   (wordfeats (W::morph (:forms (-vb) :nom W::return)))
   (SENSES
;    ((LF-PARENT ONT::GO-BACK)
    ((LF-PARENT ONT::RESTART)
     (example "he returned to watching tv")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
;     (TEMPL agent-templ)
     (TEMPL AGENT-FORMAL-TEMPL)
     )
   #|| ((LF-PARENT ONT::RETURN)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the truck/the cargo returned to/from Avon")
     (TEMPL affected-templ)
     (PREFERENCE 0.98)
     )||#
    ((LF-PARENT ONT::RETURN)
     (example "he returned to/from Avon" "return him to the station")
     (SEM (F::Cause F::agentive) (F::Aspect F::bounded) (F::time-span F::atomic))
     (TEMPL agent-affected-xp-optional-templ)
     )
    ((lf-parent ont::giving)
     (templ agent-affected-recipient-alternation-templ)
     (example "return more results")
     (meta-data :origin plow :entry-date 20060514 :change-date nil :comments pq)
    )
    )
   )
))

