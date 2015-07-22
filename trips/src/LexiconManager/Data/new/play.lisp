;;;;
;;;; W::play
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::play
   (SENSES
    ((EXAMPLE "play the voice note")
     ; changed from ont::execute to ont::play for asma
     (meta-data :origin boudreaux :entry-date 20060424 :change-date 20111004 :comments asma)
     (LF-PARENT ONT::play)
     (templ agent-affected-xp-templ)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    )
   )
))

