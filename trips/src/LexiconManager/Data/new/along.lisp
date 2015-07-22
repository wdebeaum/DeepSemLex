;;;;
;;;; W::ALONG
;;;;

(define-words :pos W::ADV
 :words (
  (W::ALONG
   (SENSES
    ((LF-PARENT ONT::ALONG)
     (example "he traveled along the river")
     (TEMPL BINARY-CONSTRAINT-TEMPL)
     )
    ((LF-PARENT ONT::linear-extent)
     (TEMPL BINARY-CONSTRAINT-TEMPL)
     (example "he found it along the road")
     (meta-data :origin navigation :entry-date 20080904 :change-date nil :comments nil)
     (preference .97) ;; prefer trajectory sense
     )
    )
   )
))

