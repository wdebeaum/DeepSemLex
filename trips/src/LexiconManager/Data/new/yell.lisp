;;;;
;;;; W::yell
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
   (W::yell
   (wordfeats (W::morph (:forms (-vb) :past W::yelled :ing w::yelling)))
   (SENSES
    ((LF-PARENT ONT::SAY)
     (meta-data :origin step :entry-date 20080711 :change-date nil :comments nil)
     (example "He yelled that three teams are going to Delta")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-to W::s-finite)))))
     )
    ((LF-PARENT ONT::SAY)
     (example "he yelled go over there")
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% w::utt)))
     )
    ((LF-PARENT ONT::talk)
     (example "yell to him")
     (TEMPL AGENT-to-ADDRESSEE-associated-info-OPTIONAL-TEMPL )
     )
    )
   )
))

