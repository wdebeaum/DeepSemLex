;;;;
;;;; W::attack
;;;;

(define-words :pos W::v 
 :words (
  (W::attack
   (wordfeats (W::morph (:forms (-vb) :nom W::attack :nomsubjpreps (w::of w::by) :nomobjpreps (w::on))))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::criticize)
     (TEMPL agent-addressee-templ) ; like thank
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date 20090506 :comments nil :vn ("judgement-33"))
     (LF-PARENT ONT::fighting)
     (example "the army attacked the city")
     (TEMPL agent-affected-xp-templ) 
     )
    ((meta-data :origin cardiac :entry-date 20090121 :change-date nil :comments nil :vn ("judgement-33"))
     (LF-PARENT ont::fighting)
     (example "the virus attacked his immune system")
     (TEMPL agent-affected-xp-templ) 
     )
    )
   )
))

