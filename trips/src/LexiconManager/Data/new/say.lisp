;;;;
;;;; W::say
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::say
   (wordfeats (W::morph (:forms (-vb) :past W::said)))
   (SENSES
    ((LF-PARENT ONT::SAY)
     (example "He said (to me) that three teams are going to Delta")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-xp-optional-formal-TEMPL (xp (% W::PP (w::ptype (? ptp w::to)))))
     )
    
    ((LF-PARENT ONT::SAY)
     (example "he said go over there")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% w::utt)))
     )
   
    ((LF-PARENT  ONT::say)
     (example "say it to him")
     (TEMPL AGENT-THEME-to-addressee-TEMPL)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::SAY (W::OVER))
   (wordfeats (W::morph (:forms (-vb) :past W::said)))
   (SENSES
    ;;;; swier -- say the plan over.
    ((LF-PARENT ONT::REPEAT)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  ((W::SAY (W::AGAIN))
   (wordfeats (W::morph (:forms (-vb) :past W::said)))
   (SENSES
    ((LF-PARENT ONT::REPEAT)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((w::say W::again)
   (SENSES
    ((LF (W::HUH))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_NOLO-COMPRENDEZ))
     (preference .97) 
     )
    )
   )
))

