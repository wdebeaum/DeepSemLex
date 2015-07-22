;;;;
;;;; W::want
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::want
   (SENSES
    ((LF-PARENT ONT::WANT)
     (TEMPL neutral-neutral-templ)
     (EXAMPLE "I want a dog")
     )
    ((LF-PARENT ONT::WANT)
     (TEMPL neutral-theme-subjcontrol-templ)
     (EXAMPLE "I want to go")
     )
    ((LF-PARENT ONT::WANT)
     (TEMPL neutral-action-OBJCONTROL-TEMPL)
     (EXAMPLE "I want you to go")
     )
    )
   )
))

