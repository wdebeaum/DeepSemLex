;;;;
;;;; w::such
;;;;

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; others?
  ((w::such w::a)
   (SENSES
    ((meta-data :origin step :entry-date 20080612 :change-date nil :comments nil :wn ("like%3:00:00"))
     (LF-PARENT ONT::exemplifies)
     (example "such a proposal is a good candidate for funding")
     (templ central-adj-sing-templ)
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :words (
  ;; others?
  ((w::such w::as)
   (SENSES
    ((meta-data :origin step :entry-date 20080612 :change-date nil :comments nil :wn ("like%3:00:00"))
     (EXAMPLE "a proposal such as that one")
     (LF-PARENT ONT::exemplifies)
     (lf-form w::such-as)
     (templ binary-constraint-adj-templ)     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
   (w::such
   (SENSES
    ((LF-PARENT ONT::exemplifies)
     (example "such proposals are good candidates for funding")
     (templ central-adj-plur-templ)
     (preference .97) ; prefer such X as Y rule in grammar
     )
    )
   )))

(define-words :pos W::PREP :boost-word t :templ NO-FEATURES-TEMPL
 :tags (:base500)
 :words (
  (W::such
   (SENSES
    ((LF (W::such))
     (non-hierarchy-lf t))
    )
   )
))
