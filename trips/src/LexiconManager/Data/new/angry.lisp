;;;;
;;;; W::angry
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::angry
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSEs
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am angry / an angry person")
     (LF-PARENT ONT::EMOTIONAL-PROPERTY-VAL)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date nil :comments nil :wn ("happy%3:00:00"))
     (example "an angry remark")
     (LF-PARENT ONT::EMOTIONAL-PROPERTY-VAL)
     (templ central-adj-content-templ)
     (preference .98)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am angry at her")
     (LF-PARENT ONT::EMOTIONAL-PROPERTY-VAL)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::at w::about)))))
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am angry that she does that")
     (LF-PARENT ONT::EMOTIONAL-PROPERTY-VAL)
     (TEMPL ADJ-OF-CONTENT-XP-TEMPL)
     )    
    )
   )
))

