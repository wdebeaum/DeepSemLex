;;;;
;;;; W::confirm
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::confirm
   (SENSES
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::CONFIRM)
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "confirm that they arrived")
     (TEMPL AGENT-theme-XP-TEMPL (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::NEGOTIATE)
     (example "he confirmed the information with him")
     (TEMPL AGENT-result-CO-AGENT-OPTIONAL-TEMPL)
     )
    ((LF-PARENT ONT::confirm)
     (example "I confirmed it to be broken")
     (TEMPL agent-effect-affected-objcontrol-templ)
    )
    (
     (LF-PARENT ONT::CORRELATION)
     (example "The result confirmed that the gene activates the protein")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::CORRELATION)
     (example "The result confirmed the hypothesis")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     )

   ))
))
