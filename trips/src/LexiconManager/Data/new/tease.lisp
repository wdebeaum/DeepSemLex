;;;;
;;;; W::tease
;;;;

(define-words :pos W::V :TEMPL AGENT-FORMAL-XP-TEMPL
 :words (
  (W::tease
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date 20090501 :comments nil :vn ("amuse-31.1") :wn ("tease%2:32:00" "tease%2:37:00"))
     (LF-PARENT ont::provoke)
     (TEMPL AGENT-AFFECTED-FORMAL-XP-OPTIONAL-B-TEMPL (xp (% w::pp (w::ptype (? pt w::into))))) ; like annoy,bother,concern,hurt   
      )
    )
   )
))

