;;;;
;;;; W::serve
;;;;

(define-words :pos W::v 
 :tags (:base500)
 :words (
  (W::serve
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("fulfilling-13.4.1-1"))
     (LF-PARENT ONT::supply)
     (example "that company only serves paying customers")
     (SEM (F::Aspect F::Bounded) (F::Time-span F::Atomic))
     (TEMPL agent-affected-recipient-alternation-templ) ; like supply
          )
    )
   )
))

