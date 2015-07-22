;;;;
;;;; W::THEN
;;;;

(define-words 
    :pos W::adv :templ DISC-PRE-TEMPL
 :tags (:base500)
 :words (
  (W::THEN
   (SENSES
    ;; when is it conjunct and when is it sequence-position?
    ((LF-PARENT ONT::CONJUNCT)
     )
    ((LF-PARENT ONT::SEQUENCE-POSITION)
     )
    )
   )  
))

(define-words :pos W::adv :templ PPWORD-ADV-TEMPL
 :tags (:base500)
 :words (
  (W::THEN
   (wordfeats (W::ATYPE (? atype W::pre-vp W::post w::pre-vp)))
   (SENSES
    ((LF-PARENT ONT::EVENT-TIME-REL)
     (SYNTAX (W::IMPRO-CLASS (:* ONT::TIME-LOC W::THEN)))
     (PREFERENCE 0.98)
     )
    )
   )
))

(define-words :pos W::n :templ PPWORD-N-TEMPL
 :tags (:base500)
 :words (
   (W::THEN
   (SENSES
    ((LF-PARENT ONT::DATE-OBJECT)
     (PREFERENCE 0.97)
     )
    )
   )
))

