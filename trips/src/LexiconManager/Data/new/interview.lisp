;;;;
;;;; W::interview
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::interview
   (SENSES
    ((LF-PARENT ONT::interview)
     (meta-data :origin calo-ontology :entry-date 20060713 :change-date 20070828 :comments caloy3 :wn ("interview%1:10:01"))
     (example "he is going to a job interview")
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
  (W::interview
   (SENSES
    ((LF-PARENT ONT::interview)
     (example "interview the candidate about it")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-addressee-associated-information-templ)
     )
    ;; have to have this straight transitive sense for passive to work
    ((LF-PARENT ONT::interview)
     (example "interview the candidate")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-addressee-templ)
     )
    ((LF-PARENT ONT::interview)
     (example "he was interviewing all day")
     (SEM (F::Aspect F::unbounded) (F::Time-span F::extended))
     (TEMPL agent-templ)
     (meta-data :origin csli-ts :entry-date 20070320 :change-date nil :comments nil :wn nil)
     )
    )
   )
))

