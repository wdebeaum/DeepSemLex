;;;;
;;;; W::third
;;;;

(define-words :pos W::ORDINAL :boost-word t :templ ORDINAL-TEMPL
 :words (
  (W::third
   (SENSES
    ((LF (W::NTH 3))
     (non-hierarchy-lf t)(SYNTAX (W::agr ?a)(w::ntype (? nt w::fraction w::day)))
     )
    )
   )
))

