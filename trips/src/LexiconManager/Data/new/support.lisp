;;;;
;;;; W::support
;;;;

(define-words :pos W::V 
 :words (
  (W::support
   (wordfeats (W::morph (:forms (-vb) :nom W::support)))
   (SENSES
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("admire-31.2") :wn ("support%2:31:04" "support%2:41:00" "support%2:41:01"))
     (LF-PARENT ONT::experiencer-emotion)
     (TEMPL neutral-action-objcontrol-templ) ; like hate,like,love
     )
    ((meta-data :origin "verbnet-1.5" :entry-date 20051219 :change-date nil :comments nil :vn ("admire-31.2") :wn ("support%2:31:04" "support%2:41:00" "support%2:41:01"))
     (LF-PARENT ONT::experiencer-emotion)
     (TEMPL neutral-neutral-xp-templ) ; like admire,adore,appreciate,despise,detest,dislike,loathe,miss
     )
    ;; the foundation supports the building
    (
     (LF-PARENT ONT::CORRELATION)
     (example "The result supported the hypothesis")
     (SEM (F::Aspect F::stage-level) (F::Time-span F::extended))
     (TEMPL neutral-neutral-xp-templ)
     )

    )
   )
))
