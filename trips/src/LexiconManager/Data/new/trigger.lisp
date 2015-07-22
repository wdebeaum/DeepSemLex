;;;;
;;;; W::trigger
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::trigger
   (SENSES
    ((LF-PARENT ont::start)
     (templ agent-affected-xp-templ)
 ;    (SEM (F::Aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))

