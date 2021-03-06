;;;;
;;;; W::smoke
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::smoke
   (SENSES
    ((LF-PARENT ONT::cloud-like-object)
     (templ mass-pred-templ)
     (meta-data :origin calo-ontology :entry-date 20060712 :change-date nil :wn ("smoke%1:19:00" "smoke%1:22:00") :comments caloy3)
     )
    )
   )
))

(define-words :pos W::v :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
   (W::smoke
   (SENSES
    ((LF-PARENT ONT::Smoking)
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (example "does the hotel allow smoking")
     (TEMPL AGENT-TEMPL)
     )
    ((LF-PARENT ONT::Smoking)
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (example "he smoked a cigarette") ; insufficiently restricted
     (TEMPL AGENT-AFFECTED-XP-NP-TEMPL)
     )

    ((LF-PARENT ONT::emit-vapor)
     (meta-data :origin calo-ontology :entry-date 20060117 :change-date nil :comments caloy3)
     (example "the logs smoked all night") 
     (TEMPL AGENT-TEMPL)
     )
    
    )
   )
))

