
(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  (W::dismal
    (SENSES
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "an exceptional book")
     (lf-parent ont::awful-val)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     (TEMPL central-adj-templ)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (example "a wall exceptional for climbing")
     (lf-parent ont::awful-val)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     (TEMPL adj-purpose-TEMPL)
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
      (EXAMPLE "a drug exceptional for cancer")
     (lf-parent ont::awful-val)
     (TEMPL adj-purpose-implicit-XP-templ)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     )
    ((meta-data :origin cardiac :entry-date 20080508 :change-date nil :comments LM-vocab)
     (EXAMPLE "a solution good for him")
     (lf-parent ont::awful-val)
     (SEM (f::gradability +) (f::orientation ont::less) (f::intensity ont::hi))
     (TEMPL adj-affected-XP-templ)
     )
    )
   )
))

