;;;;
;;;; W::consecrate
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::consecrate
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("dub-29.3-1"))
     (LF-PARENT ONT::ritual-classification)
     (TEMPL agent-theme-xp-templ) ; like label
     )
    )
   )
))

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::consecrate
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("dub-29.3-1"))
     (LF-PARENT ONT::categorization)
     (TEMPL agent-neutral-name-optional-templ) ; like name
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("dub-29.3-1"))
     (LF-PARENT ONT::categorization)
     (TEMPL agent-theme-xp-templ) ; like label,brand,nickname,name,baptize,crown,christen,style,anoint
     )
    )
   )
))

