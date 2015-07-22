;;;;
;;;; W::VIA
;;;;

(define-words :pos W::ADV
 :words (
  (W::VIA
   (SENSES
     ;; ont::via is point, ont::along is strip
;    ((LF-PARENT ONT::ALONG)
;     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
;     )
    ((LF-PARENT ONT::obj-in-path) ;ont::via
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     )

    ((LF-PARENT ONT::BY-MEANS-OF)
     (TEMPL BINARY-CONSTRAINT-S-TEMPL)
     (EXAMPLE "via this road")
     )

    )
   )
))

