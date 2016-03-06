;;;;
;;;; W::OTHER
;;;;

#||(define-words :pos W::n :templ COUNT-PRED-TEMPL
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
))||#

(define-words :pos W::adj :templ CENTRAL-ADJ-TEMPL
 :tags (:base500)
 :words (
  (W::OTHER
   (SENSES
    ((meta-data :origin trips :entry-date 20060824 :change-date nil :comments nil :wn ("other%3:00:00"))
     (LF-PARENT ONT::other)
     (TEMPL adj-co-theme-templ)
     (SEM (F::GRADABILITY F::-))
     (SYNTAX (W::atype W::attributive-only))
     )
    )
   )
  ))

