;;;;
;;;; W::ensure
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
   (W::ensure
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     (LF-PARENT ONT::promise)
     )
    ((meta-data :origin step :entry-date 20080705 :change-date 20090501 :comments nil)
     (LF-PARENT ONT::ensure)
     (templ agent-effect-xp-templ (xp (% w::cp (w::ctype w::s-finite))))
     (example "the funding ensures that the center will stay open")
     )
    ((meta-data :origin step :entry-date 20080724 :change-date 20090501 :comments nil)
     (LF-PARENT ONT::ensure)
     (templ agent-effect-xp-templ)
     (example "the funding ensures the operation of the school")
     )
    )
   )
))

