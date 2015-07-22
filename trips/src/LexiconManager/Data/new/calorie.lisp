;;;;
;;;; W::CALORIE
;;;;

(define-words :pos W::n :templ COUNT-PRED-TEMPL
 :words (
  (W::CALORIE
   (SENSES
    ((LF-PARENT ONT::temperature-unit) ;; this is not quite right... calorie measures heat, not temperature --wdebeaum
     (TEMPL attribute-unit-TEMPL)
     (example "how many calories are in a serving of carrots")
     (META-DATA :ORIGIN nutrition :ENTRY-DATE 20050707 :CHANGE-DATE NIL
      :COMMENTS nil))))
))

