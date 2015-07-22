;;;;
;;;; W::or
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::or W::so)
   (SENSES
    ((LF-PARENT ONT::qualification)
     (LF-FORM W::approximate)
     (TEMPL binary-constraint-measure-NP-templ)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::OR
   (SENSES
    ((LF-PARENT ONT::CONJUNCT)
     )
    )
   )
))

(define-words :pos W::conj :boost-word t
 :tags (:base500)
 :words (
  (W::OR
   (SENSES
    ((LF W::OR)
     (non-hierarchy-lf t)
     (TEMPL SUBCAT-DISJ-TEMPL)
     (SYNTAX (w::seq +) (status w::indefinite))
     )
    )
   )
))

