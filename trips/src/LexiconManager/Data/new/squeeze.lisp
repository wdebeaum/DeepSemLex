;;;;
;;;; W::squeeze
;;;;

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::squeeze
   (wordfeats (W::morph (:forms (-vb) :nom w::squeeze)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("funnel-9.3-2-1"))
     (LF-PARENT ONT::unload)
     (TEMPL agent-affected-source-templ (xp (% w::pp (w::ptype (? t w::off w::from))))) ; like dump
     )
    ((LF-PARENT ONT::squeeze)
     (example "the engine squeezes gasoline from the line")
     (meta-data :origin mobius :entry-date 20080414 :change-date 20090529 :comments nil)
     (templ agent-affected-xp-templ)
     (SEM (F::ASPECT F::DYNAMIC))
     )
    )
   )
))

