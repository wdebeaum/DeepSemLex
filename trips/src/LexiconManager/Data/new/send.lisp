;;;;
;;;; W::send
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  ((W::send (W::off))
   (wordfeats (W::morph (:forms (-vb) :past W::sent)))
   (SENSES
    ((LF-PARENT ONT::SEND)
     (example "send the cargo off to avon")
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
  (W::send
   (wordfeats (W::morph (:forms (-vb) :past W::sent)))
   (SENSES
    ((lf-parent ont::send)
     (SEM (F::aspect F::bounded) (F::time-span F::atomic))
     (templ agent-affected-recipient-alternation-templ)
     (example "send him the letter " "send the letter to him")
     (meta-data :origin plow :entry-date 20051004 :change-date nil :comments nil :vn ("send-11.1-1"))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
 ((w::send (w::forth))
  (wordfeats (W::morph (:forms (-vb) :past W::sent)))
   (senses
    ((LF-PARENT ont::releasing)
     (meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil :vn ("free-78-1"))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (example "the spark plug emits a spark")
     (TEMPL agent-affected-xp-templ)
     )
    )
   )
))

