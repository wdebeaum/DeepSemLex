;;;;
;;;; W::THERE
;;;;

(define-words :pos W::pro :boost-word t :templ PRONOUN-TEMPL
 :tags (:base500)
 :words (
  (W::THERE
   (wordfeats (W::CASE W::SUB) (W::agr (? ag W::3s W::3p)) (W::sing-lf-only +))
   (SENSES
    ((LF W::EXPLETIVE)
     (non-hierarchy-lf t))
    )
   )
))

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::THERE
   (SENSES
    ((LF-PARENT ONT::there)
     (SYNTAX (W::IMPRO-CLASS ONT::relative-location)
     ))
    #||((LF-PARENT ONT::TO-LOC)
     (example "move it to there")
     (SYNTAX (W::IMPRO-CLASS ONT::LOCATION))
     (TEMPL ppword-adv-templ (xp (% W::s)))
     )
    )||#
    ))
  ))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
  ;; what's an example of this? 
  (W::THERE
   (SENSES
    ((LF-PARENT ONT::LOCATION)
     (SYNTAX (W::case W::obj))
     (PREFERENCE 0.97)
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
   ((W::there w::you w::go)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    )
   )
))

(define-words :pos W::UttWord :boost-word t :templ NO-FEATURES-TEMPL
 :words (
  ((W::there w::we w::go)
   (SENSES
    ((LF (W::ATTENTION))
     (non-hierarchy-lf t)(SYNTAX (W::SA ONT::SA_EXPRESSIVE))
     )
    )
   )
))

