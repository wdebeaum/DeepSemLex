;;;;
;;;; W::arrive
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::arrive
   (wordfeats (W::morph (:forms (-vb) :nom W::arrival)))
   (SENSES
    ((LF-PARENT ONT::ARRIVE)
     (example "the truck arrived in/at delta from rochester")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-location-optional-TEMPL (xp (% W::PP (W::ptype (? ptp w::at W::in)))))
     )
    )
   )
))
