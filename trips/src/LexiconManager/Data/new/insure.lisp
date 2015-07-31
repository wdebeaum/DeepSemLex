;;;;
;;;; W::insure
;;;;

(define-words :pos W::V :templ agent-theme-xp-templ
 :words (
  (W::insure
   (SENSES
    ((meta-data :origin chf :entry-date 20070809 :change-date nil :comments nil)
     ;;(LF-PARENT ONT::promise)
     (lf-parent ont::insure) ;; 20120524 GUM change new parent 
     )
    )
   )
))

(define-words :pos W::v :templ agent-affected-xp-templ
 :words (
  (W::insure
   (SENSES
   
    ((meta-data :origin cmap-testing :entry-date 20090929 :change-date nil :comments nil :wn-sense (1) :vn ("defend-85"))
     (LF-PARENT ONT::protecting)
     (SEM (F::Time-span F::extended))
     (example "his armor insured his safety")
     (templ agent-affected-xp-templ)
     )
    )
   )
))
