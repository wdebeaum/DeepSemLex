;;;;
;;;; W::downregulate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::downregulate
    (wordfeats (W::morph (:forms (-vb) :nom w::downregulation)))
    (senses 
     ((LF-PARENT ont::HINDERING)
;      (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL agent-affected-xp-templ)
      )  
   
   ))
))
