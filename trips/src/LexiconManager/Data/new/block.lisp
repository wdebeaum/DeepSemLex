;;;;
;;;; W::BLOCK
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::BLOCK
     (wordfeats (W::morph (:forms (-vb))))
   (SENSES
    ((LF-PARENT ONT::hindering)
     (meta-data :origin calo :entry-date 20040908 :change-date nil :comments caloy2)
     (example "the intrusion detector blocked the hacker/signal" "he blocked the door")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-xp-templ) 
     )
    ((meta-data :origin "gloss-training" :entry-date 20100217 :change-date nil :comments nil)
     (LF-PARENT ONT::hindering)
     (TEMPL AGENT-EFFECT-AFFECTED-OBJCONTROL-TEMPL (xp (% w::cp (w::ctype w::s-from-ing))))
     (example "It blocks him from doing something")
     )
    )
   )
))

