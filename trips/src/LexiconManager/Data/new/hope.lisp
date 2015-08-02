;;;;
;;;; W::hope
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::hope
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("wish-62"))
     (LF-PARENT ONT::want)
     (TEMPL experiencer-theme-xp-templ (xp (% w::cp (w::ctype (? ctp w::s-that w::s-to))))) ; like wish
     (PREFERENCE 0.96) ;; prefer noun sense?
     )
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("wish-62"))
     (LF-PARENT ONT::want)
     (TEMPL experiencer-theme-xp-templ (xp (% w::pp (w::ptype w::for)))) ; like wish
     (PREFERENCE 0.96)
     )
    )
   )
))

