;;;;
;;;; W::tend
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::tend
   (SENSES
    ((LF-PARENT ONT::be-inclined)
     (example "he tends to speed")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL experiencer-theme-SUBJCONTROL-TEMPL (xp (% W::cp (W::ctype W::s-to))))
     )
        )
   )
))

