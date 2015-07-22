;;;;
;;;; W::IS
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::IS
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pres) (W::agr W::3s))
   (SENSES
    ;;;; He is loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; he is
    ((LF-PARENT ONT::PROGRESSIVE)
     (LF-FORM W::be)
     (TEMPL AUX-NOCOMP-TEMPL)
     (SYNTAX (W::auxname W::progr) (W::changesem +))
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; it is loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (TEMPL PASSIVE-TEMPL)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     )
    ;;;; she is hungry
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL neutral-pred-xp-templ)
     )
    ;;;; It is the truck
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL neutral-neutral-equal-templ)
         )

    ;;;; .. there is a box
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (TEMPL THERE-THEME-TEMPL (xp (% w::NP (w::agr w::3s))) )
     )
    )
   )
))

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL 
 :tags (:base500)
 :words (
  (W::IS
   (wordfeats (W::morph (:forms NIL)) (W::vform W::pres) (W::agr W::3p))
   (SENSES
    ;;;; a special contraction for "there is 1.5 volts at 3" NOTE the AGR 3p!!
    ((LF-PARENT ONT::Exists)
     (LF-FORM W::be)
     (TEMPL THERE-THEME-TEMPL (xp (% w::NP (w::agr w::3p))))
     (PREFERENCE 0.92)
     (meta-data :origin bee :entry-date 20050403 :change-date nil :comments mockup-student-1)     
     )
    ))
))

