;;;;
;;;; W::mention
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::mention
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("say-37.7") :wn ("mention%2:32:00" "mention%2:32:02"))
     ;;(LF-PARENT ONT::talk)
     (lf-parent ont::mention-claim)
     (example "mention it to him")
     (TEMPL AGENT-THEME-to-addressee-TEMPL)
     )
    ((meta-data :origin monroe :entry-date 20031223 :change-date nil :comments s7)
     (LF-PARENT ONT::say)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "he mentioned that he saw her")
     (TEMPL AGENT-THEME-XP-TEMPL (xp (% W::cp (W::ctype (? c W::s-finite)))))
     )
    ;; we need a plain transitive entry so passive will work
    ((meta-data :origin monroe :entry-date 20060426 :change-date nil :comments s7)
     (LF-PARENT ONT::mention-claim)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (example "he mentioned the truck" "it was mentioned")
     (TEMPL AGENT-THEME-XP-TEMPL)
     )
    )
   )
))

