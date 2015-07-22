;;;;
;;;; W::understand
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::understand
   (wordfeats (W::morph (:forms (-vb) :past W::understood)))
   (SENSES
    ((LF-PARENT ONT::awareness)
      (meta-data :origin "trips" :entry-date 20060315 :change-date nil :comments nil :wn ("understand%2:31:00" "understand%2:31:01"))
     (example "I understand the plan")
     (SEM (F::Aspect F::Stage-level))
     (TEMPL neutral-theme-xp-templ)
     )
    ((LF-PARENT ONT::awareness)
     (SEM (F::Aspect F::Indiv-level))
     (TEMPL neutral-templ)
     (example "I understand")
     )
    )
   )
))

