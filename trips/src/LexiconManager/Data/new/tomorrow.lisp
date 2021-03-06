;;;;
;;;; W::TOMORROW
;;;;

#|
(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::TOMORROW
   (wordfeats (W::ATYPE (? atype W::pre W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
     (SYNTAX (W::IMPRO-CLASS ONT::TOMORROW))
     )
    )
   )
))
|#

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  (W::TOMORROW
   (SENSES
    (;(LF-PARENT ONT::DATE-OBJECT)
     (LF-PARENT ONT::TOMORROW)
     ;(PREFERENCE 0.97)
     )
    )
   )
))

