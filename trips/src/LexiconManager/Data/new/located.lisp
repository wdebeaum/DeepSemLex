;;;;
;;;; W::LOCATED
;;;;

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::LOCATED
   (wordfeats (W::VFORM W::PASSIVE) (W::AGR ?agr) (W::MORPH (:forms NIL)))
   (SENSES
    (;(LF-PARENT ONT::EXISTS)
     (LF-PARENT ONT::BE-AT-LOC)
     (TEMPL NEUTRAL-LOCATION-XP-TEMPL)
     )
    )
   )
))

