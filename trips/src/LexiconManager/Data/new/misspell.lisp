;;;;
;;;; w::misspell
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (w::misspell
   (senses
    ((lf-parent ont::encoding)
     (example "you misspelled the word")
     (templ agent-theme-affected-optional-templ (xp1 (% w::NP)) (xp2 (% w::pp (w::ptype w::for))))	     
     (meta-data :origin task-learning :entry-date 20050912 :change-date nil :comments nil)
    )
   ))
))

