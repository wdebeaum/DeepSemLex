;;;;
;;;; W::RESULT
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   (W::RESULT
   (SENSES
    ((meta-data :origin calo :entry-date 20031229 :change-date 20070521 :comments html-purchasing-corpus)
     (LF-PARENT ONT::result)
     (example "here is the list of results")
     (TEMPL OTHER-RELN-TEMPL)
     )
    )
   )
))

(define-words :pos W::v 
 :words (
  ((W::result W::in)
   (SENSES
    ((EXAMPLE "The stimulation results in the activation")
     (lf-parent ont::cause-produce-reproduce) 
     (TEMPL agent-affected-xp-templ)
     )

    ((EXAMPLE "The stimulation results in the cat jumping")
     (lf-parent ont::cause-effect) 
     (TEMPL agent-EFFECT-AFFECTED-OBJCONTROL-TEMPL (xp (% W::CP (W::ctype W::s-from-ing) (w::vform w::ing))))
     )
    )
   )
))
