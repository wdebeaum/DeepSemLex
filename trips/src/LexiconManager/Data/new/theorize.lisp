;;;;
;;;; W::theorize
;;;;

(define-words :pos W::V 
 :words (
  (W::theorize
   (wordfeats (W::morph (:forms (-vb) :nom W::theory)))
   (SENSES
    (
     (LF-PARENT ONT::HYPOTHESIZE)
     (SEM (F::Aspect F::stage-level))
     (example "We theorize the gene activates the protein")
     (TEMPL neutral-formal-as-comp-templ (xp (% W::cp (W::ctype W::s-finite))))
     )
    (
     (LF-PARENT ONT::HYPOTHESIZE)
     (SEM (F::Aspect F::stage-level))
     (example "We theorize this theory")
     (TEMPL neutral-neutral-xp-templ)
     )

    )
   )
))
