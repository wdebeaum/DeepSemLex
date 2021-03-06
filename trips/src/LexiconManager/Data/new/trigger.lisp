;;;;
;;;; W::trigger
;;;;

(define-words :pos W::v 
 :words (
  (W::trigger
   (wordfeats (W::morph (:forms (-vb) :nom w::trigger)))
   (SENSES
;    ((LF-PARENT ont::start)
;;     (templ agent-affected-xp-templ)
;     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL)
;;    (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
;     )
    ((LF-PARENT ont::start)
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     (example "The rain triggered the migration of mice toward the building.")
     )    
    )
   )
))

