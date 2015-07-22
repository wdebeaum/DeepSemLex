;;;;
;;;; W::terminal
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::terminal
   (SENSES
    ((LF-PARENT ONT::religious-facility)
     (meta-data :origin calo-ontology :entry-date 20060609 :change-date nil :wn ("terminal%1:06:00") :comments plow-req)
     (EXAMPLE "going to the bus terminal")
     )
    )
   )
))

(define-words :pos w::N 
 :words (
;; Various electrical parts
  (w::terminal
  (senses((LF-parent ONT::Device-component) 
	    (templ part-of-reln-templ)
	    (meta-data :origin bee :entry-date 20040407 :change-date 20040607 :comments (test-s portability-experiment))
	    ))
  ;; Myrosia added terminal and holder for portability experiment
)
))

