;;;;
;;;; W::swing
;;;;

(define-words :pos W::v :templ AGENT-AFFECTED-XP-TEMPL
 :words (
  (W::swing
   (wordfeats (W::morph (:forms (-vb) :past W::swung :pastpart W::swung :ing W::swinging)))
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("swing%2:35:00" "swing%2:38:02" "swing%2:38:03"))
     (LF-PARENT ONT::move-back-and-forth)
     (TEMPL affected-templ) ; like move,bounce
      )
;    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("roll-51.3.1") :wn ("swing%2:35:00" "swing%2:38:02" "swing%2:38:03"))
;     (LF-PARENT ONT::rotate)
;     (TEMPL agent-affected-xp-templ) ; like rotate,turn,spin
;     (PREFERENCE 0.96)
;     )
     ;;;; the driver swung by the hospital
    ((EXAMPLE "the driver swung by the hospital")
     (LF-PARENT ONT::swing-by)
     (preference .98)
     (SEM (F::Cause F::Agentive) (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-affected-XP-TEMPL (xp (% W::pp (W::ptype W::by))))
     )
        )
   )
))

