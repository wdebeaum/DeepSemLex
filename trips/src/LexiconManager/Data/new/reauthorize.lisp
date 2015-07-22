;;;;
;;;; W::reauthorize
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::reauthorize
   (wordfeats (W::morph (:forms (-vb) :nom W::reauthorization)))
   (SENSES
    ((EXAMPLE "reauthorize him to move")
     (LF-PARENT ONT::ALLOW)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-effect-affected-objcontrol-templ (xp (% W::cp (W::ctype W::s-to))))
     (meta-data :origin task-learning :entry-date 20050909 :change-date nil :comments nil)
     )
    ((EXAMPLE "reauthorize the purchase")
     (LF-PARENT ONT::allow)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (TEMPL agent-effect-xp-templ)
     (meta-data :origin task-learning :entry-date 20050909 :change-date nil :comments nil)
     )
    )
   )
))

