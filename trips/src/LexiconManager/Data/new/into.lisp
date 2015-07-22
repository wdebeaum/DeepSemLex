;;;;
;;;; W::INTO
;;;;

(define-words :pos W::ADV
 :tags (:base500)
 :words (
  (W::INTO
   (SENSES
    ;; this sense requires trajectory +
    #||((LF-PARENT ONT::to-loc)
     (example "move it into the triangle") 
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )||#
    ((LF-PARENT ONT::goal-as-containment)
     (example "build it into the triangle")
     (meta-data :origin fruitcarts :entry-date 20050427 :change-date nil :comments fruitcart-11-4)
     (TEMPL BINARY-CONSTRAINT-S-or-np-TEMPL)
     )
    )
   )
))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::INTO
   (SENSES
    ((LF (W::INTO))
     (non-hierarchy-lf t))
    )
   )
))

