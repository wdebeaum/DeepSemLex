;;;;
;;;; W::BE
;;;;

(define-words :pos W::v :boost-word t :templ AGENT-THEME-XP-TEMPL
 :tags (:base500)
 :words (
  (W::BE
   (wordfeats (W::morph (:forms NIL)) (W::vform W::base) (W::agr ?agr))
   (SENSES
    ;;;; ...be loading a truck
    ((LF-PARENT ONT::PROGRESSIVE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::be)
     (TEMPL PROG-TEMPL)
     )
    ((LF-PARENT ONT::PROGRESSIVE)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (LF-FORM W::be)
     (TEMPL AUX-NOCOMP-TEMPL)
     (SYNTAX (W::auxname W::progr) (W::changesem +))
     )
    ;;;; ... be loaded.
    ((LF-PARENT ONT::PASSIVE)
     (LF-FORM W::be)
     (meta-data :origin trips :entry-date nil :change-date 20073003 :comments csli-revision)
     (TEMPL PASSIVE-TEMPL)
     )

    ;;;; .. be happy
    ((LF-PARENT ONT::HAVE-PROPERTY)
     (LF-FORM W::be)
     (TEMPL neutral-pred-xp-templ)
     (preference .98) ;; slighly disprefered to favor passive constructions over adjectives
     )
    ;;;; .. be the best
    ;;;; I set a lower preference to prefer EXISTS readings for sentences like "It is there" JFA 3/03
    (;;(LF-PARENT ONT::IN-RELATION)
     (lf-parent ont::be) ;; 20120524 GUM change new parent
     (LF-FORM W::be)
     (TEMPL neutral-neutral-equal-templ)
     ;;(PREFERENCE 0.96)
     )

   
    
    ;;;; .. there is a box
    ((LF-PARENT ONT::EXISTS)
     (LF-FORM W::be)
     (TEMPL THERE-theme-TEMPL)
     (preference .98)
     )
    )
   )
))

