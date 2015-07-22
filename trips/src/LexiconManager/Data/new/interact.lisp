;;;;
;;;; W::interact
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::interact
   (wordfeats (W::morph (:forms (-vb) :nom W::interaction :nomsubjpreps (w::of w::between) :nomobjpreps (w::between))))
   (SENSES
    ((LF-PARENT ONT::INTERACT)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "aspirin interacts with celebrex")
     (TEMPL AGENT-WITH-CO-AGENT-XP-TEMPL (xp (% W::PP (W::ptype W::with))))
     )

    ((LF-PARENT ONT::INTERACT)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (example "This and that interact.")
     (TEMPL AGENT-PLURAL-TEMPL)
     )

    ; BIND-INTERACT applicable only to MOLECULAR-PARTs
    ((LF-PARENT ONT::BIND-INTERACT)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (example "This protein interacts with that protein.")
     (TEMPL affected-affected-as-comp-TEMPL (xp (% W::PP (W::ptype W::with))))
     )

    ((LF-PARENT ONT::BIND-INTERACT)
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (example "This protein and that protein interact.")
     (TEMPL AFFECTED-PLURAL-TEMPL)
     )

    )
   )
))

