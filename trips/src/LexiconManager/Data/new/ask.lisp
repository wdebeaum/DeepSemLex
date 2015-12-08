;;;;
;;;; W::ask
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::ask
   (SENSES
    (;;(LF-PARENT ONT::QUESTIONING)
     ;;(lf-parent ont::ask-query-question) ;; 20120524 GUM change new parent
     (LF-PARENT ASK-QUESTION)
     (example "ask him the question")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected-iobj-theme-templ)
     )
    (;;(LF-PARENT ONT::request)
     (lf-parent ont::ask) ;; 20120524 GUM change new type
     (example "ask him to do it")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-ADDRESSEE-effect-OBJCONTROL-REQ-TEMPL)
     )
    (;;(LF-PARENT ONT::QUESTIONING)
     ;;(lf-parent ont::ask-query-question) ;; 20120524 GUM change new parent
     (LF-PARENT ASK-QUESTION)
     (example "ask if there is enough time to go along the coast")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::cp (W::ctype W::s-if))))
     )
    ((EXAMPLE "ask (him) about it")
     ;;(LF-PARENT ONT::QUESTIONING)
     ;;(lf-parent ont::ask-query-question) ;; 20120524 GUM change new parent
     (LF-PARENT ASK-QUESTION)
     (example "ask him about it")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-OPTIONAL-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL)
     )
    ((EXAMPLE "ask (him) for something")
     ;;(LF-PARENT ONT::REQUEST)
     (lf-parent ont::ask) ;; 20120524 GUM change new type
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-OPTIONAL-ADDRESSEE-THEME-TEMPL)
     )
    (;;(LF-PARENT ONT::request)
     (lf-parent ont::ask) ;; 20120524 GUM change new type
     (example "he asked that you send this message")
     (meta-data :origin joshua :entry-date 20080905 :change-date nil :comments nil)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (TEMPL AGENT-effect-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-finite w::s-that-subjunctive)))))
     )
    )
   )
))

