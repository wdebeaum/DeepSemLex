;;;;
;;;; W::pronunciation
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::pronunciation
   (SENSES
    ((LF-PARENT ONT::encoding)
     (TEMPL other-reln-theme-templ (xp (% W::pp (W::ptype (? pt W::of)))))
     (EXAMPLE "what is the pronunciation of that word")
     (meta-data :origin task-learning :entry-date 20050815 :change-date nil :wn ("pronunciation%1:10:00" "pronunciation%1:10:01") :comments nil)
     )
    )
   )
))

