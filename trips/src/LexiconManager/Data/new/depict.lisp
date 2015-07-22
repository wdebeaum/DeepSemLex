;;;;
;;;; W::depict
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::depict
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090501 :comments nil :vn ("characterize-29.2"))
     (LF-PARENT ONT::categorization)
     (TEMPL agent-neutral-as-theme-optional-templ) ; like interpret,classify
     )
   ((LF-PARENT ONT::categorization)
    (TEMPL agent-neutral-theme-objcontrol-templ)
    (EXAMPLE "it depicts him standing on a turtle")
    )
   ))
  ))

