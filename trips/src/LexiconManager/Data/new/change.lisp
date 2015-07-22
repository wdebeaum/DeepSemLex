;;;;
;;;; W::change
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::change
   (SENSES
    ((meta-data :origin caloy2 :entry-date 20050425 :change-date nil :wn ("change%1:11:00") :comments projector-purchasing)
     (LF-PARENT ONT::adjust)
     (example "there was a change in the weather")
     (templ other-reln-affected-templ  (xp (% W::pp (W::ptype (? pt w::in)))))
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::change
     (wordfeats (W::morph (:forms (-vb) :nom w::change)))
   (SENSES
    ((LF-PARENT ONT::event-of-change)
     (example "change the plan")
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     )
    ((LF-PARENT ONT::event-of-change)
     (example "change the color to grey")
     (TEMPL AGENT-affected-RESULT-TEMPL (xp (% w::pp (w::ptype (? tt w::to w::into)))))
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (meta-data :origin fruitcarts :entry-date 20050331 :change-date nil :comments fruitcarts-11-1 :vn ("turn-26.6-1"))   
     )
    (
     (lf-parent ont::burn-out-light-up-change) ;; GUM change new parent 20121030
     (TEMPL AGENT-affected-RESULT-TEMPL (xp (% w::pp (w::ptype w::to))))
     (Example "Change the knob to VDC")
     (meta-data :origin bee :entry-date 20040412 :change-date nil :comments test-s)    
     )
     ((meta-data :origin step :entry-date 20080626 :change-date nil :comments nil)
      (LF-PARENT ONT::change)
      (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
      (example "it changed in color / with time" "the room changed")
      (templ affected-theme-xp-optional-templ  (xp (% W::PP (W::ptype (? pt w::in W::with)))))
      )
   ))
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  ((W::change (w::over)) ; not sure this works with morphology -- wdebeaum
   (SENSES
    ((meta-data :origin "verbnet-2.0" :entry-date 20060315 :change-date nil :comments nil :vn ("convert-26.6.2-1"))
     (LF-PARENT ONT::replacement)
 ; like switch
     )
    )
   )
))

