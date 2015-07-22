;;;;
;;;; W::establish
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::establish
     (wordfeats (W::morph (:forms (-vb) :nom w::establishment)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("indicate-76-1-1"))
     (LF-PARENT ONT::confirm)
     (example "establish the likelihood of the story")
     (TEMPL agent-theme-xp-templ) ; like reveal,prove
     (PREFERENCE 0.96)
     )
    ((LF-PARENT ONT::establish)
     (example "establish an organization on principle")
     (SEM (F::Aspect F::Indiv-level) (F::Time-span F::extended))
     (TEMPL agent-affected-create-manner-optional-templ)
     )
    ((LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    ((LF-PARENT ONT::correlation)
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     )

    )
   )
))

