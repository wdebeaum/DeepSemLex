;;;;
;;;; W::Due
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::Due
   (SENSES
    ((LF-PARENT ONT::scheduled-time-modifier)
     (example "the proposal is due to be completed by friday")
     (meta-data :origin calo-ontology :entry-date 20060712 :change-date nil :wn ("due%3:00:00") :comments caloy3)
     )
    )
   )
))

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :words (
  ((W::due w::to)
   (SENSES    
    ((LF-PARENT ONT::due-to)
     (TEMPL binary-constraint-s-or-NP-templ)
     (Example "measuring voltage indicates where state changes due to a damaged bulb")
     (meta-data :origin beetle :entry-date 20081111 :change-date nil :comments nil)
     )
    ))
))

