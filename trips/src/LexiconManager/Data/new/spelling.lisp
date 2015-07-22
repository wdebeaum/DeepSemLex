;;;;
;;;; W::spelling
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::spelling
   (SENSES
    ((LF-PARENT ONT::encoding)
     (TEMPL other-reln-theme-templ (xp (% W::pp (W::ptype (? pt W::of)))))
     (EXAMPLE "check the spelling of that word")
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :wn ("spelling%1:10:00") :comments nil)
     )
    )
   )
))

