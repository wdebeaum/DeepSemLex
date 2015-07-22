;;;;
;;;; W::OTHER
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :tags (:base500)
 :words (
  (W::OTHER
   (SENSES
    ((meta-data :origin monroe :entry-date 20031219 :change-date nil :comments s14)
     (LF-PARENT ONT::referential-sem)
     (preference .97) ;; prefer adjectival sense
     )
    )
   )
))

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::OTHER
   (SENSES
    ;;;;; ((LF-PARENT LF_IDENTITY-VAL) (TEMPL ADJ-SUBCAT-OBJECT-TEMPL (XP (% PP (PTYPE THAN)))))
    ;;;;; ((LF-PARENT LF_IDENTITY-VAL) (TEMPL attributive-only-adj-templ))
    ;;;; Myrosia 2003-11-04 added "comparative +" to make "the other three drugs" work
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("other%3:00:00"))
     (LF-PARENT ONT::IDENTITY-VAL)
     (TEMPL adj-theme-templ)
     (SEM (F::GRADABILITY F::-))
     (SYNTAX (W::atype W::attributive-only) (W::comparative +))
     )
    )
   )
))

