;;;;
;;;; W::upregulate
;;;;

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :words (
   (W::upregulate
    (wordfeats (W::morph (:forms (-vb) :nom w::upregulation)))
    (senses 
     ((LF-PARENT ont::cause-stimulate)
;      (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
      (TEMPL agent-affected-xp-templ)
      )  
   
   ))
))
