;;;;
;;;; W::explain
;;;;

(define-words :pos W::v :templ AGENT-neutral-XP-TEMPL
 :words (
  (W::explain
    (wordfeats (W::morph (:forms (-vb) :nom w::explanation)))
   (SENSES
    ((LF-PARENT ONT::explain)
     (example "he explained the book")
     (meta-data :origin calo :entry-date 20041103 :change-date 20090506 :comments caloy2)
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     )
    )
   )
))

