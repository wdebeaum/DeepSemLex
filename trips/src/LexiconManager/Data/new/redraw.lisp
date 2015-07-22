;;;;
;;;; W::redraw
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::redraw
   (wordfeats (W::morph (:forms (-vb) :past W::redrew :past W::redrawn)))
   (SENSES
    ;;;; !!!??? Redraw the plan
    ((LF-PARENT ONT::REVISE)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

