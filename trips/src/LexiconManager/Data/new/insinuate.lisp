;;;;
;;;; W::insinuate
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
 (W::insinuate
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("say-37.7"))
     ;;(LF-PARENT ONT::announce)
     (lf-parent ont::assert)
     (TEMPL agent-theme-xp-templ (xp (% w::cp (w::ctype w::s-that)))) ; like disclose
     (PREFERENCE 0.96)
     )
    
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("say-37.7"))
     ;;(LF-PARENT ONT::announce)
     (lf-parent ont::assert)
     (TEMPL agent-theme-xp-templ) ; like respond,reply
     (PREFERENCE 0.96)
     )
    )
   )
))

