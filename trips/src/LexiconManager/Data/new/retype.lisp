;;;;
;;;; W::retype
;;;;

(define-words :pos W::v :templ AGENT-THEME-XP-TEMPL
 :words (
(W::retype
    (SENSES
    ((EXAMPLE "let me retype it")
     ;;(LF-PARENT ONT::type)
     (lf-parent ont::author-write-burn-print_reprint_type_retype_mistype) ;; 20120523 GUM change new parent
     (SEM (F::Aspect F::bounded) (F::Time-span F::extended))
     (meta-data :origin plow :entry-date 20050927 :change-date nil :comments naive-subjects)
     )
    )
   )
))

