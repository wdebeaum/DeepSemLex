;;;;
;;;; W::DRIVE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::DRIVE
   (SENSES
    ((meta-data :origin calo :entry-date 20030605 :change-date nil :wn ("drive%1:06:03") :comments calo-y1script)
     (LF-PARENT ont::io-device)
     (PREFERENCE 0.98) ; prefer compound
     )
    #|
    ((LF-PARENT ONT::driving-trip)
     (example "the drive to atlanta")
     (meta-data :origin ralf :entry-date 20040809 :change-date nil :wn ("drive%1:04:00") :comments nil)
     )
    |#
    )
   )
))

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
   ((w::drive w::shaft)
   (SENSES
    ((meta-data :origin LbR :entry-date 20080922 :change-date nil :comments nil)
     (LF-PARENT ONT::DEVICE)
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-affected-XP-TEMPL
 :tags (:base500)
 :words (
  (W::DRIVE
   (wordfeats (W::morph (:forms (-vb) :past W::drove :pastpart W::driven :nom w::drive)))
   (SENSES
    ;;;; Drive the cargo to Avon
    ((meta-data :origin trips :entry-date 20060414 :change-date nil :comments nil :vn ("drive-11.5") :wn ("drive%2:38:02"))
     (LF-PARENT ONT::DRIVE)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     )
    ;;;; Drive to Avon
    ((LF-PARENT ONT::DRIVE)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL AGENT-TEMPL)
     )

    ((meta-data :origin mobius :entry-date 20070728 :change-date nil :comments engines)
     (LF-PARENT ONT::cause-to-move)
     (templ agent-affected-xp-templ)
     )

    (
     (LF-PARENT ONT::cause-effect) 
     (SEM (F::Aspect F::bounded) (F::Time-span F::atomic))
     (TEMPL agent-affected-effect-objcontrol-pred-templ)  
     (meta-data :origin medadvisor :entry-date 20011126)
     (example "it drove him crazy")
     )

    (;(LF-PARENT ONT::control-manage)
     (LF-PARENT ONT::cause-effect)
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-affected-xp-templ)
     (example "the principle drives the execution")
     )

    )
   )
))

