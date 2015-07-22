(in-package :om)

(define-type ONT::ROOT
 )

(define-type -
 :parent ONT::ROOT
 )

(define-type ONT::ANY-SEM
 :parent ONT::ROOT
 )

(define-type +
 :parent ONT::ANY-SEM
 )

;;; This is for pronouns like "it" and "that", which normally don't refer to times.
(define-type ONT::referential-sem
 :wordnet-sense-keys ("entity%1:03:00")
 :parent ONT::ANY-SEM
 :sem ((? rst F::phys-obj F::abstr-obj F::situation F::proposition))
 ;; defining optional number role to suppress warnings from the om code when transforming
 ;; ont::referential-sem :number (2) -- the :number is added in the grammar
;; :arguments ((:OPTIONAL ONT::number)
 ;;            )
 )

;;; THE MAIN CORE TYPES in HIERARCHY
(define-type ONT::PHYS-OBJECT
 :wordnet-sense-keys ("object%1:03:00" "physical_object%1:03:00")
;; :parent ONT::ANY-SEM
 :parent ONT::referential-sem
 :sem (F::Phys-obj (:default (F::intentional -)))
 ;; Myrosia 2003/11/08 commented out the argument
 ;; Spatial-loc applies to physical objects
 ;; But we don't want to have a preference for it over events, or we get too many bads like
 ;; There are (people here)
 ;; :arguments ((:OPTIONAL ONT::SPATIAL-LOC (F::Phys-obj))
 ;;            )
 )

;;; this is the atachment point for verbs
(define-type ONT::situation-root
  :parent ont::referential-sem ; ONT::any-sem -- changing to referential-sem for reference; it, that can refer to situations
  :sem (F::Situation (F::Intentional -) (F::information F::mental-construct) (F::container -))
  :arguments ((:optional ont::arg0)  ;; abstract role for robust processing
	      (:optional ont::arg1)   ;; abstract role for robust processing
	      (:optional ont::norole)
	      (:optional ont::spatial-loc);;  any event can be spatially located
             )
 )

;;  events that involve change or force - i.e., either an :agent or :affected role, or both
(define-type ont::event-of-change 
     :parent ONT::SITUATION-ROOT
     :arguments ((:optional  ONT::agent ((? cau2 F::situation F::Abstr-obj f::phys-obj)))
		 (:optional  ONT::affected ((? cau2 F::situation F::Abstr-obj f::phys-obj)))
		 (:optional  ONT::result ((? cau2 F::situation F::Abstr-obj f::phys-obj)))
		 (:optional ONT::beneficiary ((? cau1 f::phys-obj))))
     :sem (F::Situation (F::aspect F::dynamic)))

;;  events of event occurrence - e.g., an explosion happened
(define-type ont::occurring
     :parent ONT::SITUATION-ROOT
     :arguments ((:essential ONT::neutral (f::situation (F::aspect F::dynamic))))
     :sem (F::Situation (F::aspect F::dynamic)))

;; events that involve :agent
(define-type ont::event-of-action 
     :parent ONT::event-of-change
     :sem (F::Situation (F::cause F::force))
     :arguments ((:essential ONT::agent ((? cau2 F::situation F::Abstr-obj f::phys-obj)))))

(define-type ont::event-of-agent-interaction 
     :parent ONT::event-of-action
     :sem (F::Situation)
     :arguments ((:essential ONT::agent1 ((? cau3 F::situation F::Abstr-obj f::phys-obj)))))


(define-type ont::event-of-awareness
     :parent ONT::event-of-change
     :sem (F::Situation) ;; (F::cause F::Mental))
     :arguments ((:essential ONT::formal ((? cau4 F::situation F::Abstr-obj)))))

;;  small class of events of change that don't take an :agent
(define-type ont::event-of-undergoing-action
     :parent ONT::event-of-change
     :sem (F::Situation)
     :arguments ((:essential ONT::affected  ((? aff F::Abstr-obj f::phys-obj)))))

;;  takes both :agent and :affected
(define-type ont::event-of-causation 
     :parent ONT::event-of-action
     :sem (F::Situation)
     :arguments ((:essential ONT::affected ((? cau5 F::Abstr-obj f::phys-obj f::situation)))))



(define-type ont::event-of-creation
     :parent ONT::event-of-action
     :sem (F::Situation)
     :arguments ((:optional ONT::result ((? neu F::situation F::Abstr-obj f::phys-obj)))
		 (:optional ONT::affected-result ((? neu F::situation F::Abstr-obj f::phys-obj))))
     )

(define-type ont::event-of-state 
     :parent ONT::SITUATION-ROOT
     :sem (F::Situation (F::aspect f::static))
     :arguments ((:essential ONT::neutral)
		 (:essential ONT::formal ((? neu F::situation F::Abstr-obj)))))




;;; I've left this in for compatability until I clean up the hierarchy JFA 9/01
;; event nouns
(define-type ONT::EVENT-TYPE
 :parent ONT::SITUATION-ROOT
 :sem (F::Situation (F::intentional -))
 )

(define-type ONT::ABSTRACT-OBJECT
 :wordnet-sense-keys ("psychological_feature%1:03:00" "abstraction%1:03:00" "abstract_entity%1:03:00")
;; :parent ONT::ANY-SEM
  :parent ont::referential-sem
 :sem (F::abstr-obj)
 )

(define-type ont::abstract-object-nontemporal
    :parent ont::abstract-object
    :sem (F::abstr-obj)
   )

(define-type ont::mental-construction
    :parent ont::abstract-object
    :sem (F::abstr-obj (f::information f::mental-construct))
   )

(define-type ont::abstract-object-property
    :parent ont::abstract-object
    :sem (F::abstr-obj)
   )

;;; part can be part of anything
(define-type ONT::Part
 :wordnet-sense-keys ("part%1:24:00" "portion%1:24:00" "component_part%1:24:00" "component%1:24:00" "constituent%1:24:00" "part%1:17:00" "piece%1:17:00")
;; :parent ONT::ANY-SEM
  :parent ont::referential-sem
 :arguments ((:OPTIONAL ONT::OF)
             )
 )


