;;;;
;;;; W::excoriate
;;;;

(define-words :pos W::v :templ agent-theme-xp-templ
 :words (
  (W::excoriate
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::defame)
     (TEMPL agent-addressee-templ) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090508 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::defame)
     (example "he excoriated it")
     (TEMPL agent-theme-xp-templ) 
     )
    )
   )
))

