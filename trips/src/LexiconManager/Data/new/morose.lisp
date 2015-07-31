;;;;
;;;; W::morose
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::morose
   (wordfeats (W::morph (:FORMS (-ER -LY))))
   (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am morose / a morose person")
     (LF-PARENT ONT::EMOTIONAL-PROPERTY-VAL)
     (templ central-adj-experiencer-templ)
     )
    ((meta-data :origin adj-devel :entry-date 20080926 :change-date nil :comments nil :wn ("happy%3:00:00"))
     (example "a morose disposition") ;; not a stimulus!
     (LF-PARENT ONT::EMOTIONAL-PROPERTY-VAL)
     (templ central-adj-content-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "I am morose about it")
     (LF-PARENT ONT::EMOTIONAL-PROPERTY-VAL)
     (TEMPL ADJ-THEME-XP-TEMPL (xp (% W::PP (w::ptype (? pt w::about)))))
     )
    )
   )  
))
