;;;;
;;;; w::dawn
;;;;

(define-words :pos W::n
 :tags (:base500)
 :words (
  (w::dawn
  (senses;;;;; night is separate because we can have it with or without articles
   ((LF-PARENT ONT::recurring-time-of-day)
    (SEM (F::time-function (? tf F::day-period f::day-point)))
    (templ time-reln-templ)
     )
   )
)
))

