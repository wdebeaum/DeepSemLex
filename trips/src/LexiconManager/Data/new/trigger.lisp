;;;;
;;;; W::trigger
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::trigger
   (SENSES
    ((LF-PARENT ont::start)
;     (templ agent-affected-xp-templ)
     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL)
;    (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    ((LF-PARENT ont::start)
     (TEMPL agent-effect-xp-templ)
     )
    
    )
   )
))

