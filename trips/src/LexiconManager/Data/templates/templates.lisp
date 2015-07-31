;;;;;
;;;;;;; templates.lisp
;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :lxm)

(define-templates
    ( (null
       (ARGUMENTS
	))

      (AGENT-THEME-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::FORMAL)
	))

      (agent-formal-xp-templ
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LCOMP (:parameter xp (:default (% W::CP  (W::ctype W::s-finite)))) ONT::FORMAL)
	))

      ;; he told me the story, He confided in me that he was sick
      (agent-affected-xp-optional-formal-templ
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::np  (w::sort (? !xx W::unit-measure))))) ont::affected optional)
	(LCOMP (% W::CP  (W::ctype W::s-finite)) ONT::FORMAL)
	))

      (affected-THEME-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::affected)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::FORMAL)
	))

      (affected-THEME-between-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::affected)
	(LOBJ (:parameter xp (:default (% W::ADVBL (W::lf (% ?p (w::class ont::between))))
					  )) ONT::FORMAL)
	))

      (AGENT-THEME-XP-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::FORMAL optional)
	))
      
      (AGENT-PROPERTY-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::PROPERTY)
	))
      
      #||(AGENT-THEME-VALUE-XP-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::AGENT)
      (LOBJ (% W::NP) ONT::FORMAL)
      (LCOMP (:parameter xp (:default (% W::PP (w::ptype W::at)))) ONT::VALUE)
      ))||#

      (AGENT-goal-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::RESULT)
	))

      (AGENT-goal-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::RESULT optional)
	))

      (theme-goal-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ont::formal)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::RESULT)
	))
      (INSTRUMENT-THEME-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::INSTRUMENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::FORMAL)
	))

      (INSTRUMENT-affected-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::INSTRUMENT)
	(LOBJ (:parameter xp (:default (% W::NP (w::sort (? !xx W::unit-measure))))) ONT::affected)
	))
      
      
      (AGENT-AFFECTED-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP  (w::sort (? !xx W::unit-measure))))) ONT::AFFECTED)
	))

      (AFFECTED-affected-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::affected)
	(LOBJ (:parameter xp (:default (% W::NP  (w::sort (? !xx W::unit-measure))))) ONT::AFFECTED1)
	))

      (AFFECTED-affected-as-comp-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::AFFECTED1)
	))

      (neutral-formal-as-comp-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::neutral)
	(LCOMP (:parameter xp (:default (% W::NP))) ONT::formal)
	))

      (AGENT-neutral-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP  (w::sort (? !xx W::unit-measure))))) ONT::neutral)
	))

      (AGENT-AFFECTED-XP-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::AFFECTED optional)
	))

      (AGENT-AFFECTED-nogerund-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP (w::gerund -) (w::sort (? !xx W::unit-measure))) ONT::AFFECTED)
	))

      (AGENT-AFFECTED-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP (w::sort (? !xx W::unit-measure))))) ONT::AFFECTED optional)
	))
      
      (AGENT-AFFECTED-THEME-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::AFFECTED)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::FORMAL)
	))

      (AGENT-AFFECTED-for-recipient-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::AFFECTED)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::RESULT optional)
	))

      (AGENT-AFFECTED-THEME-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::AFFECTED)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::of)))) ONT::FORMAL OPTIONAL)
	))

      (AGENT-AFFECTED-effect-subjObjcontrol-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::affected)
    	(LCOMP (% W::NP (W::gap ?gap) (w::gerund +))
	       ont::formal))
	)

      (AGENT-AFFECTED-THEME-Objcontrol-pred-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar) (w::sort (? !xx W::unit-measure))) ONT::affected)
    	(LCOMP (% W::PRED (W::filled -) (W::gap ?gap) (W::argument (% W::np (W::sem ?dobjsem)  (W::var ?dobjvar)(W::lex ?dobjlex))))
	       ont::formal)
	))

      (AGENT-AFFECTED-effect-Objcontrol-pred-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar) (w::sort (? !xx W::unit-measure))) ONT::affected)
    	(LCOMP (% W::PRED (W::filled -) (W::gap ?gap) (W::argument (% W::np (W::sem ?dobjsem) (W::lex ?dobjlex) (W::var ?dobjvar))))
	       ont::formal)
	))

     
      (AFFECTED-EFFECT-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::AFFECTED)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::formal)
	))

      (agent-AFFECTED-EFFECT-TEMPL  ;; we switch these to AGENT as a general role in reduced role set
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::Affected)
	(LCOMP (:parameter xp (:default (% w::pp (w::ptype w::from)))) ONT::formal)
	))

     
      (CAUSE-AFFECTED-EFFECT-xp-TEMPL  ;; we switch these to AGENT as a general role in reduced role set
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (w::sort (? !xx W::unit-measure))) ONT::Affected)
	(LCOMP (:parameter xp (:default (% w::pp (w::ptype w::from)))) ONT::formal)
	))

      (CAUSE-RESULT-AFFECTED-XP-TEMPL  ;; we switch these to AGENT as a general role in reduced role set
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::Result)
	(LCOMP (:parameter xp (:default (% w::pp (w::ptype w::to)))) ONT::Affected)
	))

      (agent-RESULT-AFFECTED-XP-TEMPL  ;; we switch these to AGENT as a general role in reduced role set
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::Result)
	(LCOMP (:parameter xp (:default (% w::pp (w::ptype w::to)))) ONT::Affected)
	))

      (agent-RESULT-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::Result)
	))

      #||

      (THEME-CAUSE-optional-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::FORMAL)
      (LOBJ (:parameter xp (:default (% W::PP (w::ptype w::in)))) ONT::agent optional)
      ))||#

      (AGENT-EFFECT-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::FORMAL OPTIONAL)
	))

      (AGENT-EFFECT-TEMpl
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::FORMAL)
	))

      (CAUSE-EFFECT-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::formal)
	))

      #||
      (INSTRUMENT-THEME-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::INSTRUMENT)
      (LOBJ (% W::NP) ONT::FORMAL)
      ))

      (INSTRUMENT-affected-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::INSTRUMENT)
      (LOBJ (% W::NP) ONT::affected)
      ))

      (INSTRUMENT-PROPERTY-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::INSTRUMENT)
      (LOBJ (% W::NP) ONT::PROPERTY)
      ))
      ||#

      (AGENT-affected-INSTRUMENT-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::PP (w::ptype w::with)))) ONT::affected1 optional)
	))

      (AGENT-INSTRUMENT-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (:parameter xp (:default (% W::PP (w::ptype w::with)))) ONT::instrument)
	))

      (THEME-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::FORMAL)
	))

      ;; the subject of unaccusative verbs is not agentive and the grammar can treat it as the object, e.g. in gerunds
      ;; e.g. "the swelling of the ankle" should get , similar to the analagous transitive "the loading of the truck"
      (THEME-unaccusative-TEMPL
       (SYNTAX (w::unaccusative +))
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::FORMAL)
	))

      (affected-unaccusative-TEMPL
       (SYNTAX (w::unaccusative +))
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::affected)
	))

      (AGENT-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	))

      (affected-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::affected)
	))

      (AGENT-TIME-DURATION-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::Time-duration-rel OPTIONAL)
	))

					; 04/13/06 ont::entity role changed to ont::formal
					;  (entity-TEMPL
					;   (ARGUMENTS
					;    (LSUBJ (% W::NP) ONT::entity)
					;    ))
      (theme-value-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ont::formal)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::value)
	))

      (neutral-theme-xp-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::NEUTRAL)
	(LOBJ (:parameter xp (:default (% W::NP))) ont::formal)
	))

      (neutral-formal-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::NEUTRAL)
	(LOBJ (:parameter xp (:default (% W::NP))) ont::formal)
	))

      (neutral-extent-xp-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::NEUTRAL)
	(LOBJ (:parameter xp (:default (% W::NP))) ont::extent)
	))

      (THEME-COST-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ont::formal)
	(LOBJ (% W::NP) ONT::Cost)
	))

      (THEME-DURATION-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ont::formal)
	(LOBJ (% W::NP) ONT::DURATION)
	))

      (neutral-DURATION-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ont::formal)
	(LOBJ (% W::NP) ONT::DURATION)
	))

      (neutral-theme-COMPLEX-no-SUBJMAP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::neutral)
	(LOBJ (% W::NP) ONT::cost)
	(LCOMP (% W::cp (W::ctype W::s-to)) theme)
	))

      (neutral-AFFECTED-theme-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::neutral)
	(LOBJ (% W::NP) ONT::Affected OPTIONAL)
	(LCOMP (% W::NP) ont::formal)
	))

      (neutral-AFFECTED-xp-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::neutral)
	(LOBJ (:parameter xp (:default (% W::PP (W::ptype W::to)))) ONT::Affected)
	))
      #||
      (THEME-AFFECTED-COST-EXPLETIVE-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP (W::sem ($ -)) (W::lex W::it)) NOROLE)
      (LIOBJ (% W::NP) ONT::Affected OPTIONAL)
      (LOBJ (% W::NP) ONT::Cost)
      (LCOMP (% W::cp (W::ctype W::s-to)) ont::formal)
      ))

      (THEME-COST-COMPLEX-SUBJ-GAP-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP (var ?lsubjvar) (sem ?lsubjsem)) NOROLE)
      (LOBJ (% W::NP) ONT::Cost)
      (LCOMP (% W::cp (W::ctype W::s-to) 
      (gap (% NP (var ?lsubjvar) (sem ?lsubjsem)))) ont::formal)
      ))

      (AFFECTED-COST-COMPLEX-SUBJCONTROL-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP (var ?lsubjvar) (sem ?lsubjsem) (lex ?lsubjlex)) ONT::AFFECTED)
      (LOBJ (% W::NP) ONT::Cost)
      (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) 
      (:required (W::subj (% W::np (W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var ?lsubjvar))))
      )
      ONT::FORMAL)
      ))
      (THEME-AFFECTED-DURATION-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ont::formal)
      (LOBJ (% W::NP) ONT::Affected OPTIONAL)
      (LCOMP (% W::NP) ONT::DURATION)
      ))||#
      (neutral-AFFECTED-DURATION-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::neutral)
	(LOBJ (% W::NP) ONT::Affected OPTIONAL)
	(LCOMP (% W::NP) ONT::DURATION)
	))

      (theme-AFFECTED-DURATION-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ont::formal)
	(LOBJ (% W::NP) ONT::Affected OPTIONAL)
	(LCOMP (% W::NP) ONT::DURATION)
	))

      (THEME-DURATION-EXPLETIVE-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::sem ($ -)) (W::lex W::it)) NOROLE)
	(LIOBJ (% W::NP) ont::formal OPTIONAL)
	(LOBJ (% W::NP) ONT::DURATION)
	(LCOMP (% W::cp (W::ctype W::s-to)) ont::formal)
	))

      (AFFECTED-DURATION-EXPLETIVE-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::sem ($ -)) (W::lex W::it)) NOROLE)
	(LIOBJ (% W::NP) ONT::Affected OPTIONAL)
	(LOBJ (% W::NP) ONT::DURATION)
	(LCOMP (% W::cp (W::ctype W::s-to)) ont::formal)
	))
      
      
      (AFFECTED-DURATION-COMPLEX-SUBJCONTROL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (var ?lsubjvar) (sem ?lsubjsem) (lex ?lsubjlex)) ONT::AFFECTED)
	(LOBJ (% W::NP) ONT::DURATION)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) 
			   (:required (W::subj (% W::np (W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var ?lsubjvar))))
			   )
	       ONT::FORMAL)
	))

      (neutral-DURATION-COMPLEX-SUBJCONTROL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (var ?lsubjvar) (sem ?lsubjsem) (lex ?lsubjlex)) ONT::neutral)
	(LOBJ (% W::NP) ONT::DURATION)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) 
			   (:required (W::subj (% W::np (W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var ?lsubjvar))))
			   )
	       ONT::FORMAL)
	))

      (AGENT-COST-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::Cost)
	))

      (AGENT-COST-NEUTRAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::Cost)
	(LCOMP (:parameter xp (:default (% W::NP))) ont::neutral)
	))
      
      (NEUTRAL-COST-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::NEUTRAL)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::COST)
	))

      (neutral-BENEFICIARY-COST-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::NEUTRAL)
	(LOBJ (% W::NP) ONT::BENEFICIARY)
	(LCOMP (:parameter xp (:default (% W::NP))) ONT::COST)
	))

      (AGENT-COST-theme-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::Cost)
	(LCOMP (:parameter xp (:default (% W::NP))) ont::formal)
	))

      (neutral-COST-theme-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::neutral)
	(LOBJ (% W::NP) ONT::Cost)
	(LCOMP (:parameter xp (:default (% W::NP))) ont::formal)
	))

      (AGENT-DURATION-THEME-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::Duration)
	(LCOMP (:parameter xp (:default (% W::NP))) ONT::FORMAL)
	))
      #||
      (AGENT-THEME-COST-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::AGENT)
      (LOBJ (% W::NP) ONT::FORMAL)
      (LCOMP (:parameter xp (:default (% W::NP))) ONT::COST)
      ))||#

      
          
      (THEME-COST-EXPLETIVE-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::sem ($ -)) (W::lex W::it)) NOROLE)
	(LOBJ (% W::NP) ONT::Cost)
	(LCOMP (% W::cp (W::ctype W::s-to)) ont::formal)
	))
      
      (THEME-COST-EXPLETIVE-IOBJ-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::sem ($ -)) (W::lex W::it)) NOROLE)
	(LIOBJ (% W::NP) ONT::FORMAL1)
	(LOBJ (% W::NP) ONT::Cost)
	(LCOMP (% W::cp (W::ctype W::s-to) (W::subj ?liobj)) ont::formal OPTIONAL)
	))
      
					; 04/13/06 ont::entity role changed to ont::formal
					;  (EXPLETIVE-ENTITY-TEMPL
					;   (ARGUMENTS
					;    (LSUBJ (:parameter xp1 (:default (% W::NP)) (:required(W::sem ($ -)))) NOROLE)
					;    (LOBJ (:parameter xp2 (:default (% W::NP))) ONT::ENTITY)
					;    ))


      
      (EXPLETIVE-THEME-PRED-TEMPL
       (ARGUMENTS
	(LSUBJ (:parameter xp1 (:default (% W::NP)) (:required (W::sem ($ -)))) NOROLE)
	(LOBJ (:parameter xp2 (:default (% W::NP))) ONT::NEUTRAl)
    ;;;;; the arg of the pred will be the subject of the verb
	(LCOMP (% W::PRED (W::arg ?lobjvar) (W::filled -) (W::argument ?lobj) (W::gap ?gap)) ONT::PROPERTY)
	))

      (EXPLETIVE-THEME-TEMPL
       (ARGUMENTS
	(LSUBJ (:parameter xp1 (:default (% W::NP)) (:required(W::sem ($ -)))) NOROLE)
	(LOBJ (:parameter xp2 (:default (% W::CP (w::ctype w::s-finite)))) ONT::NEUTRAL)
	))

      ;; used only for "there is ..." constructions. 
      (THERE-theme-TEMPL  
       (ARGUMENTS
	(LSUBJ (% W::NP (W::sem ($ -)) (W::lex w::there)) NOROLE)
	(LOBJ (:parameter xp (:default (% W::NP (w::agr (? a w::1s w::1p w::2s w::2p w::3s w::3p -)))))
	      ONT::NEUTRAL)
	))
      
      
      ;; it rained
      (EXPLETIVE-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (w::expletive +) (W::sem ($ -))) NOROLE)
	))
      
      (THEME-ALONG-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::affected)
	(LOBJ (% W::NP) ONT::ALONG)
	))
      #||
      (AGENT-ALONG-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::aGENT)
      (LOBJ (% W::NP) ONT::ALONG)
      ))||#
      
      (GO-FROM-TO-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::affected)
	(LOBJ (:parameter xp1 (:default (% W::NP))) ONT::source OPTIONAL)
	(LCOMP (:parameter xp2 (:default (% W::PP (W::ptype W::to)))) ont::result OPTIONAL)
	))

      (GO-TO-FROM-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::affected)
	(LOBJ (:parameter xp1 (:default (% W::PP (W::ptype w::in)))) ont::result OPTIONAL)
	(LCOMP (:parameter xp2 (:default (% W::PP (W::ptype W::from)))) ONT::source OPTIONAL)
	))
      
      (LOCATION-THEME-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::LOCATION)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::FORMAL)
	))
      
      (AGENT-SOURCE-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::SOURCE)
	))
      
      (AGENT-SOURCE-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::to)))) ONT::source OPTIONAL)
	))

      (AGENT-PATH-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::PATH)
	))
      
      (AGENT-SOURCE-affected-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::SOURCE)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::of)))) ONT::affected OPTIONAL)
	))

      (AGENT-AFFECTED-EFFECT-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::AFFECTED)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::of)))) ONT::formal OPTIONAL)
	))

      (agent-THEME-AFFECTED-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (:parameter xp1 (:default (% W::NP))) ONT::FORMAL)
	(LCOMP (:parameter xp2 (:default (% W::pp (W::ptype W::for)))) ONT::AFFECTED OPTIONAL)
	))

      (agent-effect-AFFECTED-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::formal)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::AFFECTED OPTIONAL)
	))
     
      (AGENT-affected-TO-LOC-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::PP (w::ptype w::to)))) ont::result)
	))
     
      (agent-TO-LOC-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LCOMP (:parameter xp (:default (% W::PP (w::ptype w::to)))) ont::result)
	))
      (AGENT-GOAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::RESULT OPTIONAL)
	))

      (AGENT-AFFECTED-GOAL-TO-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::PP (W::ptype W::to)))) ONT::RESULT)
	))

     
      (AGENT-AFFECTED-GOAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::ADVBL (W::lf (% ?p (w::class (? x ont::goal-reln ont::position-reln ont::source-reln))))
					   )))
			  
	       ONT::RESULT)
	))

      (AGENT-AFFECTED-EFFECT-loc-objcontrol-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP  (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::ADVBL (W::lf (% ?p (w::class (? x ont::goal-reln ont::position-reln ont::source-reln))))
					   (w::arg ?dobjvar)
					   (W::argument (% W::S (W::sem ?dobjsem)  (W::var ?dobjvar) (W::lex ?dobjlex))))))
	       ONT::result)
	))

      (AGENT-EFFECT-loc-subjcontrol-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::sem ?subjsem)  (W::var ?subjvar) (W::lex ?subjlex)) ONT::agent)
	(LCOMP (:parameter xp (:default (% W::ADVBL (W::lf (% ?p (w::class (? x ont::goal-reln ont::position-reln ont::source-reln))))
					   (w::arg ?subjvar)
					   (W::argument (% W::S (W::sem ?subjsem)  (W::var ?subjvar) (W::lex ?subjlex))))))
	       ONT::result)
	))

     (AGENT-AFFECTED-goal-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::ADVBL (W::lf (% ?p (w::class (? x ont::goal-reln ont::position-reln))))))
			   )
	       ONT::RESULT optional)
	))

      (AGENT-AFFECTED-loc-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::ADVBL (W::lf (% ?p (w::class ont::position-reln)))))
			   )
	       ont::spatial-loc optional)
	))

      (AGENT-neutral-GOAL-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::neutral)
	(LCOMP (:parameter xp (:default (% W::PP (W::ptype W::to)))) ONT::RESULT optional)
	))

      (AGENT-cost-GOAL-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::cost)
	(LCOMP (:parameter xp (:default (% W::PP (W::ptype W::to)))) ONT::RESULT optional)
	))
     
      (AFFECTED-cause-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::var ?subjvar)) ONT::AFFECTED)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::agent)
	))
      (AFFECTED-cause-XP-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::var ?subjvar)) ONT::AFFECTED)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::agent optional)
	))

      (AFFECTED-result-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::var ?subjvar)) ONT::AFFECTED)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::result)
	))

      (neutral-neutral-equal-templ
       (ARGUMENTS
      	(LSUBJ (% W::NP (W::agr ?agr)(w::gerund -)) ONT::neutral)
	(LOBJ (% W::NP (W::agr ?agr) (w::gerund -)) ONT::neutral1)
	))

      ;; this is a classic stative template 
      (THEME-PRED-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::var ?subjvar) (W::lex ?lsubjlex)) ONT::neutral)
    ;;;;;(argument ?lsubj)
    ;;;;; the arg of the pred will be the subject of the verb
	(LOBJ (:parameter xp (:default (% W::PRED (W::arg ?subjvar))) (:required(W::filled -) (W::argument ?lsubj) 
										(W::gap ?gap))) ONT::PROPERTY)
	))

      (neutral-PRED-xp-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::var ?subjvar) (W::lex ?lsubjlex)) ONT::neutral)
    ;;;;;(argument ?lsubj)
    ;;;;; the arg of the pred will be the subject of the verb
	(LOBJ (:parameter xp (:default (% W::PRED (W::arg ?subjvar))) (:required(W::filled -) (W::argument ?lsubj) 
										(W::gap ?gap))) ONT::FORMAL)
	))

      (AFFECTED-PRED-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::var ?subjvar) (W::lex ?lsubjlex)) ONT::AFFECTED)
       ;;;;; the arg of the pred will be the subject of the verb
	(LOBJ (:parameter xp (:default (% W::PRED (W::arg ?subjvar))) (:required(W::filled -) (W::argument ?lsubj) 
										(W::gap ?gap))) ONT::PROPERTY)
	))


      (agent-PRED-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::var ?subjvar) (W::lex ?lsubjlex)) ONT::agent)
    ;;;;; the arg of the pred will be the subject of the verb
	(LOBJ (:parameter xp (:default (% W::PRED (W::arg ?subjvar))) 
			  (:required (W::filled -) 
				     (W::argument (% W::np (W::sem ?lsubjsem) (W::lex ?lsubjlex)
						     (W::var ?lsubjvar))) 
				     (W::gap ?gap))) ONT::PROPERTY)
	))
      
            
      ;; select red
      (agent-theme-PRED-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::PRED) ONT::FORMAL)
	))
      
        (THEME-PLURAL-TEMPL
       (ARGUMENTS

	(LSUBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::FORMAL)
	))

      (AGENT-PLURAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::AGENT)
	))
      
    ;;;;; to be used when there is a plural "affected"
      (AFFECTED-PLURAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::AFFECTED)
	))
      #||
      (AGENT-THEME-PLURAL-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::AGENT)
      LOBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::FORMAL)
      ))
      
      (AGENT-THEME-MASS-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::AGENT)
      (LOBJ (% W::NP (W::mass w::mass)) ONT::FORMAL)
      ))
      ||#
      (agent-neutral-PLURAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::neutral)
	))

(agent-neutral-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP) ONT::neutral)
	))

(agent-neutral-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::neutral optional)
	))

      (agent-affected-PLURAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::affected)
	))

         ;; for "divide 7 into 21"
      (AGENT-THEME-THEME-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::FORMAL1)
	(LCOMP (:parameter xp (:default (% W::PP (w::ptype w::into)))) ONT::FORMAL)
	))

      (AGENT-THEME-THEME-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::FORMAL)
	(LCOMP (:parameter xp (:default (% W::PP (w::ptype w::with)))) ONT::FORMAL1 optional)
	))
      #||
      (cognizer-THEME-CO-THEME-OPTIONAL-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::agent)
      (LOBJ (% W::NP) ONT::FORMAL)
      (LCOMP (:parameter xp (:default (% W::PP (w::ptype w::with)))) ONT::FORMAL1 optional)
      ))||#

      (AGENT-THEME-SUBJCONTROL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::AGENT)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
											    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ONT::FORMAL)
	))

    
      (AGENT-Affected-theme-SUBJCONTROL-TEMPL
       (ARGUMENTS
    ;;;;; (LSUBJ (% NP) AGENT)
	(LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::AGENT)
    ;;;;; (LCOMP (:parameter xp (:default (% cp (ctype s-to))) (:required (subj ?lsubj))) THEME)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
											    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ONT::FORMAL)
	))

 (AGENT-Affected-THEME-SUBJCONTROL-optional-TEMPL
       (ARGUMENTS
    ;;;;; (LSUBJ (% NP) AGENT)
	(LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::AGENT)
    ;;;;; (LCOMP (:parameter xp (:default (% cp (ctype s-to))) (:required (subj ?lsubj))) THEME)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
											    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ONT::FORMAL optional)
	))
      
      ;; he had a book reviewed
      (CAUSE-EFFECT-passive-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::AFFECTED optional)
	(LCOMP (:parameter xp (:default (% W::vp (W::vform W::passive)))
			   (:required (W::subj (% W::np (W::sem ?dobjsem)
						  (W::lex ?dobjlex)
						  (W::var ?dobjvar)))))
	       ONT::result)
	))
;; the leader banned him from trying
      (agent-EFFECT-AFFECTED-OBJCONTROL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::AFFECTED)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to)))
			   (:required (W::subj (% W::np (W::sem ?dobjsem)
						  (W::lex ?dobjlex)
						  (W::var ?dobjvar)))))
	       ONT::FORMAL)
	))

     ;; the leader banned him (from trying)
      (agent-EFFECT-AFFECTED-OBJCONTROL-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::AFFECTED)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to)))
			   (:required (W::subj (% W::np (W::sem ?dobjsem)
						  (W::lex ?dobjlex)
						  (W::var ?dobjvar)))))
	       ONT::FORMAL optional)
	))

      (agent-affected-theme-OBJCONTROL-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::affected)
    ;;;;; (LCOMP (:parameter xp (:default (% cp (ctype s-to))) (:required (subj ?lobj))) EFFECT)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?dobjsem) (W::lex ?dobjlex) (W::var ?dobjvar))))) ONT::FORMAL optional)
	))

    (agent-affected-theme-OBJCONTROL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::affected)
    ;;;;; (LCOMP (:parameter xp (:default (% cp (ctype s-to))) (:required (subj ?lobj))) EFFECT)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?dobjsem) (W::lex ?dobjlex) (W::var ?dobjvar))))) ONT::FORMAL)
	))
      
      (agent-theme-OBJCONTROL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::NOROLE)
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to)))
			   (:required (W::subj (% W::np (W::sem ?dobjsem)
						  (W::lex ?dobjlex)
						  (W::var ?dobjvar)))))
	       ONT::FORMAL)
	))

      (neutral-theme-OBJCONTROL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::neutral)
	(LOBJ (% W::NP (W::lex ?dobjlex) (w::sort (? !xx W::WH-DESC)) (W::var ?dobjvar)) ONT::NOROLE)  ;; no WH descriptions work
	(LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to)))
			   (:required (W::subj (% W::np (W::sem ?dobjsem)
						  (W::lex ?dobjlex)
						  (W::var ?dobjvar)))))
	       ONT::FORMAL)
	))

      #|| ;;;;; swift 11/26/01 -- add this for aspirin makes me sick
      (CAUSE-EFFECT-AFFECTED-OBJCONTROL-PRED-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::affected)
    ;;;;; the arg of the pred will be the subject of the verb
	(LCOMP (% W::PRED (W::filled -) (W::gap ?gap) (W::argument (% W::np (W::sem ?dobjsem) (W::lex ?dobjlex) 
								      (W::var ?dobjvar)))) ONT::RESULT)
	))

      (Agent-effect-COMPLEX-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (w::var ?dobjvar) (w::lex ?dobjlex) (w::sem ?dobjsem)) NOROLE)
	(LCOMP (% W::PRED (W::filled -) (W::gap ?gap) (W::argument (% W::np (W::sem ?dobjsem) (W::lex ?dobjlex) 
								      (W::var ?dobjvar)))) ONT::EFFECT)
	))||#

      (Agent-theme-complex-subjcontrol-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP  (w::var ?subjvar)(w::lex ?dobjlex) (w::sem ?dobjsem))  ONT::agent)
	(LCOMP (% W::PRED (W::filled -) (W::gap ?gap) (W::argument (% W::np (W::sem ?subjsem) (W::lex ?subjlex) 
								      (W::var ?subjvar)))) ont::formal)
	))

      (neutral-theme-complex-subjcontrol-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP  (w::var ?subjvar)(w::lex ?dobjlex) (w::sem ?dobjsem))  ONT::neutral)
	(LCOMP (% W::PRED (W::filled -) (W::gap ?gap) (W::argument (% W::np (W::sem ?subjsem) (W::lex ?subjlex) 
								      (W::var ?subjvar)))) ont::formal)
	))

(AGENT-neutral-complex-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::norole)
    ;;;;; the arg of the pred will be the object of the verb
	(LCOMP (:parameter xp (:default (% W::PRED (W::filled -) (W::gap ?gap) (W::argument (% W::np (W::sem ?dobjsem) (W::lex ?dobjlex) 
											      (W::var ?dobjvar)))))) ont::formal)
	))

      (AGENT-affected-OBJCONTROL-PRED-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::affected)
    ;;;;; the arg of the pred will be the subject of the verb
	(LCOMP (:parameter xp (:default (% W::PRED (W::filled -) (W::gap ?gap) (W::argument (% W::np (W::sem ?dobjsem) (W::lex ?dobjlex) 
											      (W::var ?dobjvar)))))) ONT::RESULT)
	))

      #||
      (CAUSE-EFFECT-XP-TEMPL
      (ARGUMENTS
      (LSUBJ (% W::NP) ONT::agent)
      (LOBJ (:parameter xp (:default (% W::NP))) ONT::EFFECT)
      ))
     

      (EFFECT-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::EFFECT)
	))||#
      
      (COGNIZER-RESULT-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::RESULT)
	))
      
      (COGNIZER-THEME-SOURCE-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	(LOBJ (:parameter xp1 (:default (% W::NP))) ONT::FORMAL)
	(LCOMP (:parameter xp2 (:default (% W::pp (w::ptype w::from)))) ONT::Source optional)
	))
      

      (affected-SOURCE-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::from)))) ONT::SOURCE)
	))
      
      (affected-SOURCE-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::from)))) ONT::SOURCE optional)
	))

      (AGENT-AFFECTED-SOURCE-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::AFFECTED)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::from)))) ONT::SOURCE OPTIONAL)
	))

 (affected-AFFECTED-SOURCE-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::affected)
	(LOBJ (% W::NP) ONT::AFFECTED1)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::from)))) ONT::SOURCE OPTIONAL)
	))

      (AGENT-AFFECTED2-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::AFFECTED)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::AFFECTED1)
	))

(AGENT-neutral2-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::neutral)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::neutral1)
	))

      (AGENT-AFFECTED2-optional-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::AFFECTED)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::AFFECTED1 optional)
	))
      
      (AGENT-THEME-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype (? pt w::of w::for W::into))))) ONT::FORMAL optional)
	))
      
      (AGENT-affected-SOURCE-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::from)))) ONT::SOURCE)
	))
      
      (AGENT-result-CO-AGENT-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LIOBJ (% W::NP) ONT::result)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::AGENT1 OPTIONAL)
	))

     (AGENT-AFFECTED-CO-AGENT-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LIOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::AGENT1 OPTIONAL)
	))

      (AGENT-WITH-CO-AGENT-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::AGENT1 optional)
	))

      (AGENT-WITH-CO-AGENT-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::AGENT1)
	))

      (AGENT-CO-AGENT-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ  (:parameter xp (:default (% W::NP))) ONT::AGENT1)
	))
      
      (AGENT-GOAL-THEME-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LIOBJ (% W::NP) ONT::RESULT)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::FORMAL OPTIONAL)
	))

      (GOAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::RESULT)
	))
      
      (AGENT-RECIPIENT-affected-OPTIONAL-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LIOBJ (% W::NP) ont::result)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::affected OPTIONAL)
	))

(AGENT-beneficiary-affected-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LIOBJ (% W::NP) ONT::beneficiary)
	(LCOMP (% W::NP) ONT::affected)
	))
      
      (AGENT-GOAL-THEME-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LIOBJ (% W::NP) ONT::RESULT)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::FORMAL OPTIONAL)
	))
      
;; loaded a truck with oranges
      (AGENT-GOAL-affected-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (% W::NP) ONT::affected)
	(LCOMP (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::affected OPTIONAL)
	))
      

      (COGNIZER-affected-XP-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::AGENT)
	(LOBJ (:parameter xp (:default (% W::NP))) ONT::affected)
	))
      
      (COGNIZER-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::agent)
	))
      
      (neutral-TEMPL
       (ARGUMENTS
	(LSUBJ (% W::NP) ONT::neutral)
	))
      
  ;;;;; these do change the sem features
  (AUX-MODAL-TEMPL
   (SYNTAX(W::AUX +) (W::MODAL +) (W::CHANGESEM +) (W::morph (:forms NIL)) (W::AGR ?agr))
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) NOROLE)
    (LCOMP (% W::VP- (W::vform W::base) (W::subj (% W::NP (W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var 
            ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr))) (W::roles ?croles) (W::subj-map ?subj-map 
       ) (W::tranform ?transform) (W::class ?cclass) (W::constraint ?constraint) (W::tma ?tma) (W::subjvar 
         ?subjvar) (W::dobjvar ?dobjvar)) NOROLE)
    ))
  
  (DO-TEMPL
   (SYNTAX(W::AUX +) (W::MODAL +) (W::CHANGESEM -) (W::morph (:forms NIL)) (W::AGR ?agr))
   (ARGUMENTS
    ;;;;;; (LSUBJ (% NP) norole)
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) NOROLE)
    ;;;;;; (LCOMP (% VP- (vform base) (aux -) (subj ?vpsubj) (roles ?croles)
    ;;;;;;	 (constraint ?con) (tma ?tma) (class ?c) (tranform ?transform)
    ;;;;;;	 (subjvar ?vpsubjvar) (dobjvar ?dobjvar) (subj-map ?subj-map)
    ;;;;;;	)
    ;;;;;;	 norole)
    (LCOMP (% W::VP- (W::vform W::base) (W::subj (% W::NP (W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var 
            ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr))) (W::roles ?croles) (W::subj-map ?subj-map 
       ) (W::tranform ?transform) (W::class ?cclass) (W::constraint ?constraint) (W::tma ?tma) (W::subjvar 
         ?subjvar) (W::dobjvar ?dobjvar)) NOROLE)
    ))
  
  ;;;;; for will, shall -- these don't change the sem features
  (AUX-FUTURE-TEMPL
   (SYNTAX(W::AUX +) (W::MODAL +) (W::CHANGESEM -) (W::VFORM W::FUT) (W::morph (:forms NIL)) (W::AGR 
      ?agr))
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) NOROLE)
    (LCOMP (% W::VP- (W::vform W::base) (W::subj (% W::NP (W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var 
            ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr))) (W::roles ?croles) (W::subj-map ?subj-map 
       ) (W::tranform ?transform) (W::class ?cclass) (W::constraint ?constraint) (W::tma ?tma) (W::subjvar 
         ?subjvar) (W::dobjvar ?dobjvar)) NOROLE)
    ))
  
  ;;;;; he should/could/would have gone (only takes the perfective as a complement)
  (COND-PAST-TEMPL
   (SYNTAX(W::AUX +) (W::MODAL +) (W::CHANGESEM +) (W::VFORM W::PAST) (W::morph (:forms NIL)) (W::AGR 
      ?agr))
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) NOROLE)
    (LCOMP (% W::VP- (W::vform W::base) (W::aux +) (W::modal -) (W::auxname W::PERF) (W::subj (% W::NP (
           W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr 
          ))) (W::roles ?croles) (W::subj-map ?subj-map) (W::tranform ?transform) (W::class ?cclass) (
        W::constraint ?constraint) (W::tma ?tma) (W::subjvar ?subjvar) (W::dobjvar ?dobjvar)) NOROLE)
    ))
  
  ;;;;; he should/could/would go
  (COND-PRES-TEMPL
   (SYNTAX(W::AUX +) (W::MODAL +) (W::CHANGESEM +) (W::VFORM W::PRES) (W::morph (:forms NIL)) (W::AGR 
      ?agr))
   (ARGUMENTS
    ;;;;;; (LSUBJ (% NP) norole)
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) NOROLE)
    ;;;;;; (LCOMP (% VP- (vform base) (auxname (? an PASSIVE PROGR -)) (subj ?vpsubj) (roles ?croles)
    ;;;;;;	 (constraint ?constraint) (tma ?tma) (class ?cclass) (tranform ?transform)
    ;;;;;;	 (subjvar ?vpsubjvar) (dobjvar ?dobjvar) (subj-map ?subj-map)
    ;;;;;;	)
    ;;;;;;	 norole)
    (LCOMP (% W::VP- (W::vform W::base) (W::auxname (? an W::PASSIVE W::PROGR -)) (W::subj (% W::NP (W::sem 
            ?lsubjsem) (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr))) (
        W::roles ?croles) (W::subj-map ?subj-map) (W::tranform ?transform) (W::class ?cclass) (W::constraint 
        ?constraint) (W::tma ?tma) (W::subjvar ?subjvar) (W::dobjvar ?dobjvar)) NOROLE)
    ))
  
  ;;;;; allow null complements for auxiliary verbs to handle 'I can' 'I will' 'I do'
  (MODAL-AUX-NOCOMP-TEMPL
   (SYNTAX(W::AUX +) (W::modal +) (W::ellipsis +) (W::morph (:forms NIL)) (W::AGR ?agr))
   (ARGUMENTS
    ;;;;;; (LSUBJ (% NP) THEME) ;; map subject to theme of aux
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) ont::formal
    )
    ;;;;;; (LCOMP (% VP- (vform base) (subj ?vpsubj) (roles ?croles)
    ;;;;;;	 (constraint ?constraint) (tma ?tma) (class ?cclass) (tranform ?transform)
    ;;;;;;	 (subjvar ?vpsubjvar) (dobjvar ?dobjvar) (subj-map ?subj-map)
    ;;;;;;	)
    ;;;;;;	 norole)
    (LCOMP (% W::VP- (W::vform W::base) (W::subj (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case 
            ?lsubjcase) (W::agr ?lsubjagr))) (W::roles ?croles) (W::subj-map ?subj-map) (W::tranform 
         ?transform) (W::class ?cclass) (W::constraint ?constraint) (W::tma ?tma) (W::subjvar ?subjvar) (
        W::dobjvar ?dobjvar)) NOROLE)
    ))
  
  ;;;;; allow null complements for auxiliary verbs to handle "I am" "he has"
  (AUX-NOCOMP-TEMPL
   (SYNTAX(W::AUX +) (W::modal -) (W::ellipsis +) (W::morph (:forms NIL)) (W::AGR ?agr))
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) ont::formal)
    (LCOMP (% W::VP- (W::vform W::base) (W::auxname (? an W::PASSIVE W::PROGR W::PERF)) (W::subj (% W::NP (
           W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr 
          ))) (W::roles ?croles) (W::subj-map ?subj-map) (W::tranform ?transform) (W::class ?cclass) (
        W::constraint ?constraint) (W::tma ?tma) (W::subjvar ?subjvar) (W::dobjvar ?dobjvar)) NOROLE)
    ))
  
  (PROG-TEMPL
   (SYNTAX(W::morph (:forms NIL)) (W::AUX +) (W::MODAL -) (W::CHANGESEM +) (W::AGR ?agr) (W::AUXNAME 
      W::PROGR))
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) NOROLE)
    (LCOMP (% W::VP- (W::vform W::ing) (W::subj (% W::NP (W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var 
            ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr))) (W::roles ?croles) (W::subj-map ?subj-map)
	    (W::tranform ?transform) (W::class ?cclass) (W::constraint ?constraint) (W::tma ?tma) (W::subjvar 
         ?subjvar) (W::dobjvar ?dobjvar)) NOROLE)
    ))
  
  (GONNA-TEMPL
   (SYNTAX(W::morph (:forms NIL)) (W::AUX +) (W::MODAL +) (W::CHANGESEM -) (W::AGR ?agr))
   (ARGUMENTS
    ;;;;; let this process as a modal (which, in a sense, it is), since the progressive aspect comes from 'be'
    ;;;;;(LSUBJ (% NP) norole)
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) NOROLE)
    ;;;;;; (LCOMP (% VP- (vform base) (modal -) (subj ?vpsubj) (roles ?croles) (tranform ?transform)
    ;;;;;;	 (constraint ?constraint) (tma ?tma) (class ?cclass) (subj-map ?vpsubj-map)
    ;;;;;;	 (subjvar ?vpsubjvar) (dobjvar ?dobjvar)) situation)
    (LCOMP (% W::VP- (W::vform W::base) (W::modal -) (W::subj (% W::NP (W::sem ?lsubjsem) (W::lex ?lsubjlex 
          ) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr))) (W::roles ?croles) (W::subj-map 
         ?subj-map) (W::tranform ?transform) (W::class ?cclass) (W::constraint ?constraint) (W::tma ?tma) (
        W::subjvar ?subjvar) (W::dobjvar ?dobjvar)) ONT::situation)
    ))
  
  ;;;;;; (GOING-TO-XP-TEMPL
  ;;;;;; ;; let 'go' process as a modal (which, in a sense, it is), since the progressive aspect comes from 'be', and
  ;;;;;; ;; the semantics should come from the main clause V
  ;;;;;; (SYNTAX (morph (:forms nil)) (AUX +) (MODAL +) (AGR ?agr))
  ;;;;;; (ARGUMENTS
  ;;;;;; (LSUBJ (% NP) THEME)
  ;;;;;; (LCOMP (% cp (ctype s-to) (vform base) (subj ?lsubj) (roles ?croles)
  ;;;;;;	 (constraint ?constraint) (tma ?tma) (class ?cclass) (subj-map ?subj-map)
  ;;;;;;	 (subjvar ?subjvar) (dobjvar ?dobjvar)) norole)
  ;;;;;;)
  ;;;;;; ;; (LCOMP (xp (% cp (ctype s-to))) norole)
  ;;;;;;)
  (PASSIVE-TEMPL
   (SYNTAX(W::morph (:forms NIL)) (W::AUX +) (W::MODAL -) (W::AUXNAME W::PASSIVE) (W::CHANGESEM -) (
     W::AGR ?agr))
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) NOROLE)
    (LCOMP (% W::VP- (W::vform W::passive) (W::subj (% W::NP (W::sem ?lsubjsem) (W::lex ?lsubjlex) (W::var 
            ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr))) (W::roles ?croles) (W::subj-map ?subj-map 
       ) (W::tranform ?transform) (W::class ?cclass) (W::constraint ?constraint) (W::tma ?tma) (W::subjvar 
         ?subjvar) (W::dobjvar ?dobjvar)) NOROLE)
    ))
  
  (PERFECTIVE-TEMPL
   (SYNTAX (W::AUX +) (W::MODAL -) (W::CHANGESEM +) (W::AGR ?agr) (W::AUXNAME W::PERF))
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr)) NOROLE)
    (LCOMP (% W::VP- (W::vform W::pastpart) (W::subj (% W::NP (W::sem ?lsubjsem) (W::lex ?lsubjlex)
	   (W::var ?lsubjvar) (W::case ?lsubjcase) (W::agr ?lsubjagr))) (W::roles ?croles) (W::subj-map ?subj-map) 
	   (W::tranform ?transform) (W::class ?cclass) (W::constraint ?constraint) (W::tma ?tma) (W::subjvar ?subjvar)
	   (W::dobjvar ?dobjvar)) NOROLE)
    ))

 
(agent-LOCATION-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LCOMP (:parameter xp (:default (% W::ADVBL (W::lf (% ?p (w::class (? x ont::position-reln))))))) ont::spatial-loc)
    ))

 (agent-LOCATION-optional-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LCOMP (:parameter xp (:default (% W::PP (w::ptype (? ptp w::on w::in w::under w::into w::at))))) ONT::SPATIAL-LOC optional)
    ))

(neutral-LOCATION-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::neutral)
    (LCOMP  (:parameter xp (:default (% W::PP (w::ptype (? ptp w::on w::in w::under w::into w::at))))) ONT::SPATIAL-LOC)
    ))
  
(neutral-neutral-xp-location-templ
 (ARGUMENTS
  (LSUBJ (% W::NP)  ont::neutral)
  (LOBJ  (:parameter xp1 (:default (% W::NP))) ONT::neutral1)
  (LCOMP  (:parameter xp (:default (% W::PP (w::ptype (? ptp w::on w::in w::under w::into w::at))))) ONT::SPATIAL-LOC)
  ))

  (AGENT-AFFECTED-IOBJ-THEME-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::FORMAL)
    ;;;;; maponly)
    (LIOBJ (% W::NP) ONT::AFFECTED)
    ))

 #||  ;;;;; establish the principle on truth
  (AGENT-neutral-THEME-optional-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp1 (:default (% W::NP))) ONT::FORMAL)
    (LCOMP (:parameter xp2 (:default (% W::PP (W::ptype W::on)))) ONT::property optional)
    ))||#
  
     ;;;;; base the comparison on price
  (AGENT-neutral-THEME-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp1 (:default (% W::NP))) ONT::neutral)
    (LCOMP (:parameter xp2 (:default (% W::PP (W::ptype W::on)))) ont::formal)
    ))

(AGENT-neutral-extent-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp1 (:default (% W::NP))) ONT::neutral)
    (LCOMP (:parameter xp2 (:default (% W::PP (W::ptype W::on)))) ont::extent)
    ))
  
   ;;;;; discuss it with him
  (AGENT-THEME-ADDRESSEE-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp1 (:default (% W::NP))) ONT::FORMAL)
    (LCOMP (:parameter xp2 (:default (% W::PP (W::ptype (? pt W::to W::with))))) ONT::agent1 optional)
    ))

    ;;;;; talk about it to her
  (AGENT-about-THEME-ADDRESSEE-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp1 (:default (% W::PP (W::ptype W::about)))) ONT::FORMAL)
    (LCOMP (:parameter xp2 (:default (% W::PP (W::ptype (? pt W::to W::with))))) ONT::AGENT1 optional)
    ))
  
  ;;;;; talk about it to her
  (AGENT-associated-info-ADDRESSEE-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp1 (:default (% W::PP (W::ptype W::about)))) ONT::FORMAL)
    (LCOMP (:parameter xp2 (:default (% W::PP (W::ptype (? pt W::to))))) ONT::AGENT1 optional)
    ))
  
   ;;;;; reply to her about it
  (AGENT-to-ADDRESSEE-associated-info-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp1 (:default (% W::PP (W::ptype W::to)))) ONT::agent1)
    (LCOMP (:parameter xp2 (:default (% W::PP (W::ptype (? pt W::about))))) ont::formal  optional)
    ))
  
  ;;;;; talk to her (no message)
  (AGENT-TO-ADDRESSEE-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::PP (W::ptype (? pt W::to W::with))) ONT::agent1)
    ))
  
  ;;;;; talk about it (no addressee)
  (AGENT-COMP-THEME-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LCOMP (:parameter xp (:default (% W::PP (W::ptype (? pt W::about W::for))))) ONT::FORMAL)
    ))
  
;;  we retain this one so we can identify the verbs previously identified as allowing the
;;    alternation. The alternation is not sanctioned for all verbs using the recipient construction in the grammar
 (AGENT-affected-RECIPIENT-alternation-TEMPL
  (ARGUMENTS
   (LSUBJ (% W::NP) ONT::AGENT)
   (LOBJ (% W::NP) ONT::affected)
   ;; (LIOBJ  (:parameter xp (:default (% W::NP))) ont::result)
    ))

;;;======

 (AGENT-THEME-TO-ADDRESSEE-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::FORMAL)
    (LCOMP (% W::PP (W::ptype W::to)) ONT::agent1)
    ))
 
    (AGENT-THEME-TO-ADDRESSEE-optional-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::FORMAL)
    (LCOMP (% W::PP (W::ptype W::to)) ONT::agent1 optional)
    ))

  (AGENT-neutral-TO-ADDRESSEE-optional-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::neutral)
    (LCOMP (% W::PP (W::ptype W::to)) ONT::Agent1 optional)
    ))

   (AGENT-THEME-FOR-RECIPIENT-optional-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::FORMAL)
    (LCOMP (% W::PP (W::ptype W::for)) ont::result optional)
    ))

    (AGENT-THEME-FOR-RECIPIENT-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::FORMAL)
    (LCOMP (% W::PP (W::ptype W::for)) ont::result)
    ))
  
  (AGENT-THEME-PLURAL-RECIPIENT-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::FORMAL)
    (LIOBJ (% W::NP) ont::result optional)
    ))

 (AGENT-affected-PLURAL-RECIPIENT-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::affected)
    (LIOBJ (% W::NP) ont::result optional)
    ))
  
  (AGENT-THEME-MASS-RECIPIENT-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP (W::mass w::mass)) ONT::FORMAL)
    (LIOBJ (% W::NP) ont::result optional)
    ))

(AGENT-affected-MASS-RECIPIENT-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP (W::mass w::mass)) ONT::affected)
    (LIOBJ (% W::NP) ont::result optional)
    ))

  (AGENT-TO-RECIPIENT-THEME-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::RESULT)
    (LCOMP (:parameter xp (:default (% W::NP)))  ONT::FORMAL)
    ))

;; e.g., cc me on that
  (AGENT-RECIPIENT-affected-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::AFFECTED-RESULT)
    (LCOMP (:parameter xp (:default (% W::NP)))  ONT::affected)
    ))

;; e.g., the hospital banned smoking
  (AGENT-EFFECT-XP-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::FORMAL)
    ))
  
   ;;;;; swift 24/01/02 use this to replace AGENT-BENEFICIARY-THEME-XP-TEMPL for warn, inform
  ;;;;; verbs with this template require he addressee
  (AGENT-ADDRESSEE-THEME-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp1 (:default (% W::NP))) ONT::Agent1)
    (LCOMP (:parameter xp (:default (% W::PP (w::ptype w::about)))) ont::formal OPTIONAL)
    ))

   (AGENT-ADDRESSEE-THEME-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp1 (:default (% W::NP))) ONT::Agent1)
    (LCOMP (:parameter xp (:default (% W::PP (w::ptype w::about)))) ont::formal)
    ))
  
  (AGENT-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::agent1)
    (LCOMP (:parameter xp (:default (% W::pp (W::ptype W::about)))) ont::formal)
    ))

   (AGENT-ADDRESSEE-ASSOCIATED-INFO-optional-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::agent1)
    (LCOMP (:parameter xp (:default (% W::pp (W::ptype W::about)))) ont::formal optional)
    ))
  
  (AGENT-OPTIONAL-ADDRESSEE-ASSOCIATED-INFORMATION-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::agent1 OPTIONAL)
    (LCOMP (:parameter xp (:default (% W::pp (W::ptype W::about)))) ont::formal)
    ))
  
  (AGENT-ASSOCIATED-INFORMATION-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LCOMP (:parameter xp (:default (% W::pp (W::ptype W::about)))) ont::formal)
    ))

  (neutral-ASSOCIATED-INFORMATION-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::neutral)
    (LCOMP (:parameter xp (:default (% W::pp (W::ptype W::about)))) ont::formal)
    ))
  
  ;;;;; use agent-addressee-theme instead? But here theme is obligatory and addressee optional
  ;;;;; addressee is optional : ask (for) it
  (AGENT-OPTIONAL-ADDRESSEE-THEME-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::Agent1)
    (LCOMP (:parameter xp (:default (% W::pp (W::ptype W::for)))) ont::formal OPTIONAL)
    ))
  
  ;;;;; roles have both been mapped to the person who is being addressed, depending on the syntax...
  ;;;;; this template will be for "tell/ask him", greet, address, call
  (AGENT-ADDRESSEE-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::Agent1)
    ))
  
  (AGENT-ADDRESSEE-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::Agent1 OPTIONAL)
    ))

    ;;;;; I asked/urged/requested him to go
  (agent-addressee-theme-OBJCONTROL-req-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::agent1)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required (W::subj (% W::np (W::sem ?dobjsem)
		(W::lex ?dobjlex) (W::var ?dobjvar))))) ont::formal)
    ))

  ;;; I appealed to him to do it
   (agent-to-addressee-theme-OBJCONTROL-req-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (:parameter xp1 (:default (% W::pp (W::ptype W::to)))) ont::agent1)
;;    (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::addressee)
    (LCOMP (:parameter xp2 (:default (% W::cp (W::ctype W::s-to))) (:required (W::subj (% W::np (W::sem ?dobjsem)
		(W::lex ?dobjlex) (W::var ?dobjvar))))) ont::formal)
    ))

    ;;;;; I asked/urged/requested him (to go)
   (agent-addressee-theme-OBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::agent1)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required (W::subj (% W::np (W::sem ?dobjsem)
		(W::lex ?dobjlex) (W::var ?dobjvar))))) ont::formal optional)
    ))

      ;;;;; I told him to go
  (agent-addressee-effect-OBJCONTROL-req-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::agent1)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required (W::subj (% W::np (W::sem ?dobjsem)
		(W::lex ?dobjlex) (W::var ?dobjvar))))) ONT::formal)
    ))

       ;;;;; I told him to go
  (agent-addressee-effect-OBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::agent1)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required (W::subj (% W::np (W::sem ?dobjsem)
		(W::lex ?dobjlex) (W::var ?dobjvar))))) ONT::formal optional)
    ))
  
  (AGENT-THEME-RESULT-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::FORMAL)
    (LCOMP (:parameter xp (:default (% W::PP (w::ptype w::into)))) ONT::RESULT OPTIONAL)
    ))

(AGENT-AFFECTED-RESULT-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::AFFECTED)
    (LCOMP (:parameter xp (:default (% W::PP (w::ptype w::into)))) ONT::RESULT OPTIONAL)
    ))
  
  (THEME-RESULT-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::FORMAL)
    (LCOMP (:parameter xp (:default (% W::PP (w::ptype w::into)))) ONT::RESULT OPTIONAL)
    ))

(affected-RESULT-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::affected)
    (LCOMP (:parameter xp (:default (% W::PP (w::ptype w::into)))) ONT::RESULT OPTIONAL)
    ))
  
 (AGENT-affected-RESULT-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::affected)
    (LCOMP (:parameter xp (:default (% W::NP))) ONT::RESULT)
    ))

 (AGENT-affected-RESULT-arg-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP) ONT::affected-result)
    ))

(AGENT-affected-CREATE-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ  (:parameter xp (:default (% W::NP))) ONT::affected-result)
    ))

(affected-CREATE-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::affected-result)
    ))

(AGENT-affected-CREATE-MANNER-OPTIONAL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ  (:parameter xp (:default (% W::NP))) ONT::affected-result)
    (LCOMP (:parameter xp2 (:default (% W::PP (W::ptype W::on)))) ONT::manner optional)
    ))

(AGENT-affected-CREATE-MANNER-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ  (:parameter xp (:default (% W::NP))) ONT::affected-result)
    (LCOMP (:parameter xp2 (:default (% W::PP (W::ptype W::on)))) ONT::manner)
    ))

(affected-RESULT-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::affected)
    (LOBJ (% W::NP) ONT::RESULT)
    ))

;;==========  
 
 (affected-ACTION-OPTIONAL-TEMPL
   (arguments
    (LSUBJ (% w::np) ONT::affected)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::Action optional)
    ))

(affected-ACTION-xp-TEMPL
   (arguments
    (LSUBJ (% w::np) ONT::affected)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::Action)
    ))

 (affected-theme-XP-optional-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::affected)
    (LOBJ (:parameter xp (:default (% W::NP))) ont::formal optional)
    ))

(affected-neutral-optional-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::affected)
    (LOBJ (:parameter xp (:default (% W::NP))) ont::neutral optional)
    ))
   
(AGENT-ACTION-OPTIONAL-TEMPL
 (arguments
  (LSUBJ (% w::np) ONT::AGENT)
  (LOBJ (:parameter xp (:default (% W::NP))) ONT::Action optional)
  ))

(AGENT-ACTION-XP-TEMPL
 (arguments
  (LSUBJ (% w::np) ONT::AGENT)
  (LOBJ (:parameter xp (:default (% W::NP))) ONT::Action)
  ))

      ;;;;; e.g., The computer needs to have RAM

  (AGENT-ACTION-SUBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::AGENT)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
                    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ONT::action)
    ))
  
      ;;;;; e.g., He/The computer needs to go
  (neutral-theme-SUBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::neutral)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
                    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ont::formal)
    ))

(affected-theme-SUBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::affected)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
                    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ont::formal)
    ))
    ;;;;; e.g., He's gotta go
  ;; added for gotta / CAET tea making
  (neutral-EFFECT-SUBJCONTROL-BASE-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::neutral)
    (LCOMP (:parameter xp (:default (% W::vp (W::vform W::base))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
                    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ONT::FORMAL)
    ))
  
  ;;;;; e.g., I want to go
  (neutral-ACTION-SUBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::neutral)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
                    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ONT::ACTION)
    ))


(AGENT-EFFECT-OBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::neutral)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required (W::subj (% W::np (W::sem ?dobjsem)
		(W::lex ?dobjlex) (W::var ?dobjvar))))) ONT::FORMAL)
    )
)
(AGENT-AFFECTED-EFFECT-OBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::affected)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required (W::subj (% W::np (W::sem ?dobjsem)
		(W::lex ?dobjlex) (W::var ?dobjvar))))) ONT::FORMAL)
    ))

  ;;;;; I want you to go
  (neutral-ACTION-OBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::neutral)
    (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::neutral1)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) 
		       (:required (W::subj (% W::np (W::sem ?dobjsem)
					      (W::lex ?dobjlex) (W::var ?dobjvar))))) 
	   ONT::FORMAL)
    ))

   (agent-neutral-theme-OBJCONTROL-TEMPL
    (ARGUMENTS
     (LSUBJ (% W::NP) ONT::agent)
     (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::neutral)
     (LCOMP (:parameter xp (:default (% W::vp (W::vform W::ing))) 
		       (:required (W::subj (% W::np (W::sem ?dobjsem)
					      (W::lex ?dobjlex) (W::var ?dobjvar))))) 
	    ONT::FORMAL)
     ))

(neutral-neutral-neutral-xp-templ
 (ARGUMENTS
  (LSUBJ (% W::NP) ONT::neutral)
  (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ONT::neutral1)
  (LCOMP (:parameter xp (:default (% W::np))) ont::neutral2)
  ))

(neutral-plural-templ
 (ARGUMENTS
  (LSUBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::neutral1)
  ))

(neutral-neutral-plural-templ
 (ARGUMENTS
  (LSUBJ (% W::NP) ONT::neutral)
  (LOBJ (% W::NP (W::agr (? a W::1p W::2p W::3p))) ONT::neutral1)
  ))

(neutral-neutral-templ
 (ARGUMENTS
  (LSUBJ (% W::NP) ONT::neutral)
  (LOBJ  (:parameter xp (:default (% W::NP))) ONT::neutral1)
  ))

(neutral1-neutral-templ
 (ARGUMENTS
  (LSUBJ (% W::NP) ONT::neutral1)
  (LOBJ  (:parameter xp (:default (% W::NP))) ONT::neutral)
  ))

(neutral-neutral-xp-templ
 (ARGUMENTS
  (LSUBJ (% W::NP)  ont::neutral)
  (LOBJ  (:parameter xp (:default (% W::NP))) ONT::neutral1)
  ))

 (AGENT-ACTION-OBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::AGENT)
    (LOBJ (% W::NP (W::lex ?dobjlex) (W::var ?dobjvar)) ont::neutral)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required (W::subj (% W::np (W::sem ?dobjsem)
		(W::lex ?dobjlex) (W::var ?dobjvar))))) ont::formal)
    ))

  (theme-OBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::neutral)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required (W::subj (% W::np (W::sem ?dobjsem)
		(W::lex ?dobjlex) (W::var ?dobjvar))))) ont::formal)
    ))
  
  (AGENT-EFFECT-SUBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::AGENT)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
                    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ONT::FORMAL)
    ))

(neutral-EFFECT-SUBJCONTROL-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP (W::lex ?lsubjlex) (W::var ?lsubjvar)) ONT::neutral)
    (LCOMP (:parameter xp (:default (% W::cp (W::ctype W::s-to))) (:required(W::subj (% W::np (W::sem ?lsubjsem) 
                    (W::lex ?lsubjlex) (W::var ?lsubjvar))))) ONT::FORMAL)
    ))

   

(agent-neutral-as-theme-optional-templ		
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::neutral)
    (LCOMP (:parameter xp2 (:default(% W::PP (w::ptype w::as)))) ont::formal optional)
    ))

(agent-neutral-as-theme-templ		
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::neutral)
    (LCOMP (:parameter xp2 (:default(% W::PP (w::ptype w::as)))) ont::formal)
    ))

  (agent-neutral-name-optional-templ		
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::neutral)
    (LCOMP (:parameter xp2 (:default(% W::NP))) ont::formal optional)
    ))

  (agent-neutral-name-templ		
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::neutral)
    (LCOMP (:parameter xp2 (:default(% W::NP))) ont::formal)
    ))


   (agent-neutral-adj-predicate-optional-templ		
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::neutral)
    (LCOMP (:parameter xp2 (:default(% W::ADJP (w::set-modifier -)))) ont::formal optional)
    ))

  (agent-neutral-adj-predicate-templ		
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::agent)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::neutral)
    (LCOMP (:parameter xp2 (:default(% W::ADJP (w::set-modifier -)))) ont::formal)
    ))

(neutral-neutral-adj-predicate-templ		
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::neutral)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::neutral1)
    (LCOMP (:parameter xp2 (:default(% W::ADJP (w::set-modifier -)))) ont::formal)
    ))

(neutral-neutral-adj-predicate-optional-templ		
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::neutral)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::neutral1)
    (LCOMP (:parameter xp2 (:default(% W::ADJP (w::set-modifier -)))) ont::formal optional)
    ))

;;=====

  ;; for underspecified TR verbs
  (ARG0-ARG1-XP-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::ARG0)
    (LOBJ (:parameter xp (:default (% W::NP))) ONT::ARG1)
  ;;  (LCOMP (:parameter xp (:default (% W::NP))) ONT::ARG2 optional)
    ))

 ;; for underspecified IT verbs
  (ARG0-TEMPL
   (ARGUMENTS
    (LSUBJ (% W::NP) ONT::ARG0)
    ))
  
  ;;;;; Function word templates
  (SUBCAT-S-PP-Templ
   (SYNTAX(W::SUBCAT (? W::SUB W::S W::PP)))
   (ARGUMENTS
    ))
  
  (SUBCAT-S-Templ
   (SYNTAX(W::SUBCAT (? W::SUB W::S)))
   (ARGUMENTS
    ))

  (SUBCAT-ANY-Templ
   (SYNTAX(W::SUBCAT (? W::SUB W::S W::PP W::NP W::VP)))
   (ARGUMENTS
    ))

  
  ;; USED by disjunctive conjuncts such as "or" and "nor"
  (SUBCAT-DISJ-Templ
   (SYNTAX
    (W::SUBCAT (? W::SUB W::S W::PP W::NP W::VP W::ADJP))
    (W::status w::indefinite)
    (w::agr ?agr)
    (w::operator w::one-of)
    (W::conj -) (W::disj +) 
    )
   (ARGUMENTS
    ))

  
  ;; This is a double conjunction template for constructions such as 
  ;; both ... and, either ... or, neither ... nor
  ;; In all cases the lexical entry has to set subcat2 to the proper word
  ;; here it is set to "-" intentionally to remind the entries to set it
  ;; otherwise the rules will overgenerate drastically with arbitrary words
  (SUBCAT-DOUBLE-CONJ-Templ
   (SYNTAX
    (W::SUBCAT1 (? W::SUB1 W::S W::PP W::NP W::VP W::ADJP W::ADVBL))
    (W::SUBCAT2 -)
    (W::SUBCAT3 ?wsub1)
    )
   (ARGUMENTS
    ))

  
  
  (indefinite-countable-templ
   (SYNTAX(W::MASS (? ms W::COUNT W::BARE)) (W::AGR W::3S))
   (ARGUMENTS
    ))
  
  (third-person-templ
   (SYNTAX(W::MASS ?M) (W::AGR (? A W::3S W::3P)))
   (ARGUMENTS
    ))
  
  (mass-agr-templ
   (SYNTAX(W::mass ?m) (W::agr ?a))
   (ARGUMENTS
    ))

   (mass-agr-3s-templ
   (SYNTAX(W::mass ?m) (W::agr w::3s))
   (ARGUMENTS
    ))

   (mass-agr-3p-templ
   (SYNTAX(W::mass ?m) (W::agr w::3P))
   (ARGUMENTS
    ))
    
  (wh-qtype-templ
   (SYNTAX(W::WH W::q) (W::QTYPE W::wh) (W::mass ?m) (W::agr ?agr) (W::npagr ?a))
   (ARGUMENTS
    ))
  
  ;;;;; new ones here
  ;;;;; basically an empty template, for rare combinations of features
  (quan-templ
   (SYNTAX(W::SEM ?sem))
   (ARGUMENTS
    ))

   (quan-sing-count-templ   ;; e.g., each, any, another, ...
    (SYNTAX (W::SEM ?sem) (W::QOF (% W::PP (W::PTYPE W::OF) (W::AGR W::3P) (W::MASS (? m W::COUNT w::bare)))))
    (ARGUMENTS
          ))
  
  (quan-3p-templ   ;; e.g., both, 
   (SYNTAX (W::AGR W::3p) (W::MASS W::count) (W::CARDINALITY +)
	    (W::QOF (% W::PP (W::PTYPE W::OF) (W::AGR W::3P) (W::MASS W::COUNT))))
   (ARGUMENTS
    ))
  
  (quan-mass-templ   ;; e.g., much (water), most of the water, most of the truck
   (SYNTAX (W::MASS W::MASS) (W::QOF (% W::PP (W::PTYPE W::OF) (W::AGR W::3S))));;(W::MASS W::MASS))))
   (ARGUMENTS
    ))

   (quan-bare-templ   ;; e.g., much pain
   (SYNTAX (W::MASS W::BARE) (W::QOF (% W::PP (W::PTYPE W::OF) (w::mass w::bare))))
   (ARGUMENTS
    ))

  (quan-sing-sing-templ ;; e.g., (there wasn't) much (truck/of the truck) left
    (SYNTAX (W::SEM ?sem) (W::AGR W::3s) (W::QOF (% W::PP (W::PTYPE W::OF) (W::AGR W::3S) (W::MASS W::COUNT))))
    (ARGUMENTS
          ))
  
  
  (quan-count-mass-templ  ;; e.g., all, most, some, ...
   (SYNTAX (W::MASS ?m) (W::QOF (% W::PP (W::PTYPE W::OF) (W::AGR ?agr1) (W::MASS ?m))))
   (ARGUMENTS
    ))

  (quan-than-comp  ;; e.g., more than five, more than that
   (SYNTAX (W::QCOMP (% W::PP (W::PTYPE W::THAN) (W::GAP -)))
			;;(W::SEM ($ F::ABSTR-OBJ (F::INFORMATION F::DATA)))))
  	   (W::QOF (% W::PP (W::PTYPE W::OF) (W::AGR ?agr1) (W::MASS ?m)))
   ))
  
  (quan-cardinality-templ
   (SYNTAX (W::AGR ?agr1) (W::CARDINALITY +) (W::MASS (? m1 W::count w::bare))
	   (W::QOF (% W::PP (W::PTYPE W::OF) (w::agr W::3P) (w::mass (? m2 w::count w::bare)) ))   ;; cardinality in OF should be plural, right 
	   )
   (ARGUMENTS
    ))

   (quan-cardinality-pl-templ
   (SYNTAX (W::AGR w::3P) (W::CARDINALITY +) (W::MASS (? m1 W::count w::bare))
	   (W::QOF (% W::PP (W::PTYPE W::OF) (w::agr W::3P) (w::mass (? m2 w::count w::bare)) ))   ;; cardinality in OF should be plural, right 
	   )
   (ARGUMENTS
    ))
  
  (quan-no-bare-templ
   (SYNTAX(W::NobareSpec +))
   (ARGUMENTS
    ))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;
  ;;;;; Adverb templates
  ;;;;;
  (ADJ-ADV-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% (? W::argcat ;;W::ADVBL   ;; what's an example of NO ADV?
		    W::ADJP)  (w::set-modifier -) (W::sort ?sort)) ONT::OF)
    ))

  ;; Not -- don't modify discourse adverbials
   (NEG-ADJ-ADV-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS 
    (ARGUMENT (% (? W::argcat W::ADVBL W::ADJP)  (w::set-modifier -) (W::sort (? !sort w::disc))) ONT::OF)
    ))

(NEG-ADJ-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS 
    (ARGUMENT (% (? W::argcat W::ADJP)  (w::set-modifier -) (W::sort (? !sort w::disc))) ONT::OF)
    ))
  
  (ADJ-ADV-GRADABLE-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% (? W::argcat W::ADVBL W::ADJP)  (w::set-modifier -) (W::sort ?sort) (W::gradability +)) ONT::OF)
    ))
  
  (ADV-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% W::ADVBL (W::sort ?sort)) ONT::OF)
    ))

   ;; so, too, really as intensifiers
   (NON-DISC-ADV-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS 
    (ARGUMENT (% (? W::argcat W::ADVBL)  (w::set-modifier -) (W::sort (? !sort w::disc))) ONT::OF)
    ))
  
  (BINARY-CONSTRAINT-ADV-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% W::ADVBL (W::sort W::BINARY-CONSTRAINT)) ONT::OF)
    ))
  
  ;;;;;swier -- words that grade/modify adjectives get this templ
  (ADJ-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% W::ADJP) ONT::OF)
    ))

  ;; e.g. much can only modify comparative adjectives
  ;; much better, but not much red
  (COMPARATIVE-ADJ-adv-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% (? W::argcat w::advbl W::ADJP) (w::comparative +)) ONT::OF)
    ))
  
  ;; e.g. much can only modify comparative adverbs
  ;; much more
  (COMPARATIVE-ADV-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% W::ADVBL (w::comparative +)) ONT::OF)
    ))
  
  ;;;;; for words like "more" and "less"...they take a noncomparative adj
  ;;;;; and produce a comparative adj.
  (ADJ-COMP-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE) (W::comparative +))
   (ARGUMENTS
    ;;;;; this restriction does not seem to get picked up...
    (ARGUMENT (% W::ADJ) ONT::OF)
    ))


  ;; more green / more quickly
  (ADJ-ADV-COMP-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE) (w::comparative +))
   (ARGUMENTS
    (ARGUMENT (% (? W::argcat W::ADVBL W::ADJP)  (w::set-modifier -) (W::sort ?sort)) ONT::OF)
    ))

;  this is unused and (% ?argcat) is broken for DSL
;  (OPERATOR-TEMPL
;   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE))
;   (ARGUMENTS
;    (ARGUMENT (% ?argcat) ONT::OF)
;    ))

  ;; exactly five
  (NUMBER-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE) (W::MASS W::COUNT))
   (ARGUMENTS
 ;   (ARGUMENT (% W::CARDINALITY) ONT::OF)
    (ARGUMENT (% W::NUMBER) ONT::OF)
    ))
  
  ;;;;; operators that can modifier both quanitifers (e.g., almost all) and numbers
  (QUAN-OPERATOR-TEMPL
   (SYNTAX(W::SORT W::OPERATOR) (W::ATYPE W::PRE) (W::MASS W::COUNT))
   (ARGUMENTS
    (ARGUMENT (% W::QUAN) ONT::OF)
    ))
  
  (Binary-constraint-S-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (:parameter xp (:default (% W::NP (W::case (? cas W::obj -))
					(w::gerund -)))) ONT::VAL)
    ))

   ;; e.g., I opened the door by hitting it.
   (Binary-constraint-S-subjcontrol-templ
    (SYNTAX (W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
    (ARGUMENTS
     (ARGUMENT (% W::S (w::subjvar ?subjvar)) ONT::OF)
     (SUBCAT (:parameter xp (:default (% W::VP  (W::vform W::ing) (gap ?gap) (w::subjvar ?subjvar)))) ont::val
	     )))

   (Binary-constraint-S-pp-of-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (:parameter xp (:default (% W::PP (W::ptype w::of)))) ONT::VAL)
    ))

   (Binary-constraint-S-or-NP-value-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% (? W::x W::S W::NP)) ONT::OF)
    (SUBCAT (% W::value (W::case (? cas W::obj -))) ONT::VAL)
    ))

   ;; ont::time-val is an ont::val that is f::time, e.g. 'in June' 'in the middle of the night'
   (Binary-constraint-S-or-NP-TIME-VAL-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% (? W::x W::S W::NP)) ONT::OF)
    (SUBCAT (% (? cat w::value W::NP) (W::case (? cas W::obj -))) ONT::TIME-VAL)
    ))

   (Binary-constraint-S-trajectory-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S (f::sem ($ f::situation (f::trajectory +)))) ONT::OF)
    (SUBCAT (% W::NP (W::case (? cas W::obj -))) ONT::VAL)
    ))

  (binary-constraint-PRED-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::PRED (W::arg ?subjvar)) ONT::OF)
    (SUBCAT (:parameter xp (:default (% W::NP (W::case (? cas W::obj -)) (w::gerund -)))) ONT::VAL)
    ))
  
  (binary-constraint-S-OR-NP-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% (? W::x W::S W::NP)) ONT::OF)
    (SUBCAT (:parameter xp (:default (% W::NP (W::case (? cas W::obj -))))) ONT::VAL)
    ))
  
  (binary-constraint-S-implicit-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)) (W::allow-deleted-comp +))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::NP (W::case (? cas W::obj -))) ONT::VAL)
    ))
  
  (binary-constraint-sit-val-S-decl-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::S (W::stype W::decl)) ONT::SIT-VAL)
    ))
  
  (binary-constraint-sit-val-NP-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::NP (W::case (? cas W::obj -))) ONT::SIT-VAL)
    ))

  ;; for more results
   (Binary-constraint-S-obj-val-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (:parameter xp (:default (% W::NP (W::case (? cas W::obj -))))) ONT::obj-VAL)
    ))
   
  ;; BEETLE -- state at terminals
  (binary-constraint-of-state-NP-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF-STATE) ;; can only be NP, being a state
    (SUBCAT (% W::NP (W::case (? cas W::obj -))) ONT::VAL)
    ))
  
  ;; BEETLE -- terminals in the same state
  (binary-constraint-val-state-NP-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::VAL) 
    (SUBCAT (% W::NP (W::case (? cas W::obj -))) ONT::OF-STATE)
    ))

  (binary-constraint-val-state-NP-2-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::VAL) 
    (SUBCAT (% W::NP) ONT::OF-STATE)
    ))
  
  (binary-constraint-S-decl-templ
   (SYNTAX (W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST))
	   (W::ALLOW-DELETED-COMP -)
	   )
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::S (W::stype W::decl)) ONT::VAL)
    ))

(binary-constraint-S-decl-gap-templ
   (SYNTAX (W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST))
	   (W::ALLOW-DELETED-COMP -)
	   )
   (ARGUMENTS
    (ARGUMENT (% (? W::x W::S W::NP)) ONT::OF)
    (SUBCAT (% W::S (W::stype W::decl) (GAP ?GAP)) ONT::VAL)
    ))

(binary-constraint-S-decl-templ-post-only
   (SYNTAX (W::SORT W::BINARY-CONSTRAINT) (W::ATYPE W::POST)
	   (W::ALLOW-DELETED-COMP -)
	   )
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::S (W::stype W::decl)) ONT::VAL)
    ))

 (binary-constraint-NP-decl-templ
   (SYNTAX (W::SORT W::BINARY-CONSTRAINT) (W::ATYPE  (? ATYPE W::PRE W::POST))
	   (W::ALLOW-DELETED-COMP -)
	   )
   (ARGUMENTS
    (ARGUMENT (% W::NP)  ONT::OF)
    (SUBCAT (% W::S (W::stype W::decl)) ONT::VAL)
    ))

  (binary-constraint-S-decl-it-that-templ
   (SYNTAX (W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST))
	   (W::ALLOW-DELETED-COMP -)
	   )
   (ARGUMENTS
    (ARGUMENT (% W::NP (W::lex (? lx w::that w::this W::it))) ONT::OF)
    (SUBCAT (% W::S (W::stype W::decl)) ONT::VAL)
    ))

  (binary-constraint-gerund-templ
   (SYNTAX (W::SORT W::BINARY-CONSTRAINT) 
	   (W::ATYPE (? ATYPE W::PRE W::POST))
	   (W::ALLOW-DELETED-COMP -)
	  )
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::NP (w::sort w::pred) (w::lf (% w::description (w::status w::kind)))) ONT::VAL)
    ))

  
  
  ;; This is used for double-word split adverbials, specifically if ... then ...
  (binary-constraint-S-decl-middle-word-subcat-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (:parameter xp1 (:default (% W::S (W::stype W::decl)))) ONT::VAL)    
    (SUBCAT2 (:parameter xp2 (:default (% w::word (w::lex w::then)))) NOROLE)
    ))

  
 ;; 20120502 :origin jr gloss-variant whereby
 (binary-constraint-S-or-NP-decl-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% (? ag W::S w::NP)) ONT::OF)
    (SUBCAT (% W::S (W::stype (? st W::decl w::ing))) ONT::VAL)  
    ))

  (binary-constraint-S-or-NP-general-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% (? ag W::S w::NP)) ONT::OF)
    (subcat (% (? sc W::S W::NP w::adjp)  (w::set-modifier -)) ONT::VAL)
    ))
 
  (binary-constraint-S-ing-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::VP (W::vform W::ing)) ONT::VAL)
    ))
  
  (binary-constraint-S-VPbase-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::VP (W::vform W::base)) ONT::VAL)
    ))
  
  (binary-constraint-NP-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? atype W::POST w::pre)))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (SUBCAT (:parameter xp (:default (% W::NP (W::case (? cas W::obj -))))) ONT::VAL)
    ))
  
  ;;;;; modifiers for measure phrases, e.g. 'or so'
  (binary-constraint-measure-NP-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE W::POST))
   (ARGUMENTS
    (ARGUMENT (% W::NP (W::sort W::unit-measure)) ONT::OF)
    ))
  
  (binary-constraint-NP-THEME-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE W::POST))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FORMAL)
    (SUBCAT (:parameter xp (:default (% W::NP (W::case (? cas W::obj -))))) ONT::FORMAL1)
    ))
  
  (binary-constraint-NP-PROPERTY-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE W::POST))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FORMAL)
    (SUBCAT (:parameter xp (:default (% W::NP (W::case (? cas W::obj -))))) ONT::PROPERTY)
    ))
  
  (binary-constraint-NP-implicit-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE W::POST) (W::allow-deleted-comp +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (SUBCAT (% W::NP (W::case (? cas W::obj -))) ONT::VAL)
    ))
  
  (binary-constraint-templ
   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::NP (W::case (? cas W::obj -))) ONT::VAL)
    ))
  
  (PRED-S-VP-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE (? ATYPE W::PRE W::POST W::PRE-VP)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    ))

(PRED-S-or-NP-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE (? ATYPE W::PRE W::POST W::PRE-VP)))
   (ARGUMENTS
    (ARGUMENT (% (? cat W::S w::NP)) ONT::OF)
    ))
  
  (PRED-S-VP-implicit-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE (? ATYPE W::PRE W::POST W::PRE-VP)) (W::implicit-arg +))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    ;; !!!FIXME
    ;; removing the maponly since it throws this warning: parser: warning: Bad feature-value specification NIL in rule NIL
    (SUBCAT (% W::NP) ONT::VAL) ;MAPONLY)
    ))
  
  (PRED-S-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    ))
  
  (PRED-S-implicit-templ
   (SYNTAX(W::implicit-arg +) (W::SORT W::PRED) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (SUBCAT (% W::S) ONT::VAL)
    ))
  
  (PRED-NP-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::POST))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    ))
  
  (PRED-NP-subj-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::POST))
   (ARGUMENTS
    (ARGUMENT (% W::NP (W::case W::sub)) ONT::OF)
    ))
  
  (PRED-S-PRE-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    ))
  
  (PRED-VP-PRE-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::PRE-VP))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    ))
  
  (ELSE-templ
   (SYNTAX(W::SORT W::ELSE) (W::ATYPE -))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::FORMAL)
    ))
  
  ;;;;; Pre- and post-vp advs only
  (PRED-VP-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE (? ATYPE W::PRE-VP W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    ))

  ;;;;; post-vp advs only
  (PRED-S-POST-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::POST))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    ))

   ;;;;; post-vp advs only
  (PRED-S-POST-subcat-optional-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::POST) (w::allow-deleted-comp +))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::to)))) ONT::val)
    ))

  (topic-templ
   (SYNTAX(W::SORT W::DISC) (W::SA-ID ?SA-ID) (W::ATYPE (? ATYPE W::PRE)))
   (ARGUMENTS
    (ARGUMENT (% W::UTT (W::subjvar (? !sv -)) (w::uttword -)) ONT::OF) ;; utt must have a filled subject
    (SUBCAT (:parameter xp (:default (% W::NP (W::case (? cas W::obj -))))) ONT::VAL)
    ))
  
  (disc-templ
   (SYNTAX(W::SORT W::DISC) (W::SA-ID ?SA-ID) (W::ATYPE (? ATYPE W::PRE W::POST)))
   (ARGUMENTS
    (ARGUMENT (% W::UTT (w::subjvar ?sv)) ONT::OF)
    ))
  
  (disc-pre-templ
   (SYNTAX(W::SORT W::DISC) (W::SA-ID ?SA-ID) (W::ATYPE W::PRE))
   (ARGUMENTS
    (ARGUMENT (% W::UTT (w::subjvar ?sv)) ONT::OF)
    ))
  
  (disc-post-templ
   (SYNTAX(W::SORT W::DISC) (W::SA-ID ?SA-ID) (W::ATYPE W::POST))
   (ARGUMENTS
    (ARGUMENT (% W::S) ONT::OF)
    ))
  
  ;; beetle fix
  ;; Myrosia added for cases where we need a post-utt adverbial to parse things like "Is it open as well"
  ;; use with great care only for those cases, needs to be cleaned up after we clean up handling these utts
  (disc-post-UTT-templ
   (SYNTAX(W::SORT W::DISC) (W::SA-ID ?SA-ID) (W::ATYPE W::POST))
   (ARGUMENTS
    (ARGUMENT (% W::UTT) ONT::OF)
    ))
  
  ;; This template is for "for him to come", as in "she is happy for him to come"
  ;; The "him" gets no role -- it's folded into the main sentence
  (adv-double-subcat-control-templ
   (SYNTAX (W::SORT W::DOUBLE-SUBCAT) (W::ATYPE (? ATYPE W::PRE W::POST)) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS    
    (ARGUMENT (% W::S ) ONT::OF)
    (subcat (:parameter xp1 
			(:default (% W::NP) ) 
			(:required (w::var ?subcatvar) (w::sem ?subcatsem) (w::lex ?subcatlex))) NOROLE)
    (subcat2 (:parameter xp2 
			 (:default (% W::CP (W::ctype W::s-to)))  
			 (:required (W::subj (% W::np (W::sem ?subcatsem) (W::lex ?subcatlex) (W::var ?subcatvar)))))
	     ONT::Val)
    ))


  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;; Adjective templates
  ;;;;;;
  ;;;;;; swift 02/10/03 I am in the process of phasing out the old adj templates, which make use
  ;;;;;; of an ATYPE pre-post distinction, in favor of Bob Swier's templates (below), which
  ;;;;;; make a finer-grained classification of adjectives: central (the most common type), attributive-only
  ;;;;;; (can only appear before the nominal they modify), predicative-only (can only appear in predicative
  ;;;;;; position, and post-positive (can appear directly after the nominal without a verb: e.g. trucks available).
  ;;;;;
  ;;;;; replaced by attributive-only template
  ;;;;; currently used only for 'ultimate' and 'other'
  ;;;;; (simple-adj-templ
  ;;;;; (Syntax
  ;;;;; (COMP-OP MORE)
  ;;;;; (SORT PRED)
  ;;;;; (ATYPE PRE)
  ;;;;; (SUBCAT -)
  ;;;;; (ARG ?arg)
  ;;;;;)
  ;;;;; (ARGUMENTS
  ;;;;; (ARGUMENT (% NP) LF_OF)
  ;;;;;)
  ;;;;;)
    ; 04/13/06 ont::entity role changed to ont::formal
  ;;;;; this is currently used only for 'located'

  (simple-adj-entity-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::central) (W::SUBCAT -) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::ENTITY)
    ))

  (less-adj-templ
   (SYNTAX (W::COMP-OP W::LESS) (W::SORT W::PRED) (W::ATYPE W::CENTRAL) (W::SUBCAT -) (W::ARG ?arg)
	   )
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    ))
  
   (own-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::CENTRAL) (W::ARG ?arg) (W::ALLOW-post-n1-subcat +) (W::allow-deleted-comp +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FIGURE)
    (subcat (% W::NP) ONT::GROUND)
    ))

   (compar-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::CENTRAL) (W::ARG ?arg) (W::ALLOW-post-n1-subcat +)(W::allow-deleted-comp +) )
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FIGURE)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::than)))) ONT::GROUND)
    ))

  

  (compar-subcat-required-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::CENTRAL) (W::ARG ?arg) (W::ALLOW-post-n1-subcat +)(W::allow-deleted-comp -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FIGURE)
    (subcat  (% w::pp) ONT::GROUND)
    ))

  (superl-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::CENTRAL) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FIGURE)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::than)))) ONT::GROUND)
    ))

  (adj-experiencer-theme-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE w::central) (W::ARG ?arg)  (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ont::affected)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::to)))) ont::formal)
    ))

  (adj-experiencer-theme-req-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE w::central) (W::ARG ?arg)  (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ont::affected)
    (subcat (:parameter xp (:default (% W::np))) ont::formal)
    ))
  
  (adj-CO-THEME-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::neutral)
    (subcat (:parameter xp (:default (% W::pp (W::ptype (? pt W::to w::for))))) ONT::neutral1 optional)
    ))

  
  (adj-CO-THEME-post-subcat-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::neutral)
    (post-subcat (:parameter xp (:default (% W::pp (W::ptype (? pt W::to w::for))))) ONT::neutral1)
    ))

    ;; it is available in 4 MW capacity
   (adj-subcat-property-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::IN)))) ONT::PROPERTY)
    ))
  #|| 
  ;; the task is easy for him
   (adj-THEME-cause-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FORMAL)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::CAUSE)
    ))
  ||#
   ;; This is: this store is open/closed for business route open/closed for traffic
   ;; using this template will create an optional impro even in cases such as "the store is open"
   ;; if you don't want the IMPRO, use 2 senses: central-adj-templ + adj-purpose-templ
  (adj-Purpose-optional-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::Purpose)
    ))

  ;; he is willing to go
   (adj-action-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::cp (W::ctype W::s-to)))) ONT::action)
    ))

   ;; a dog such as a collie
   (BINARY-CONSTRAINT-ADJ-TEMPL
    (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg))
    (ARGUMENTS
     (ARGUMENT (% W::NP) ONT::OF)
     (subcat (:parameter xp (:default (% W::pp (W::ptype W::as)))) ONT::val)
     ))
 
   (BINARY-CONSTRAINT-time-ADV-result-VAL-TEMPL
   (SYNTAX (w::sort w::binary-constraint)  (W::ATYPE (? ATYPE w::pre W::POST)))
   (ARGUMENTS
    (ARGUMENT (% (? ag W::S)) ONT::OF)
    (subcat (% W::ADVBL) ONT::time-VAL)
    ))
   
   (BINARY-CONSTRAINT-ADJ-result-VAL-TEMPL
   (SYNTAX (w::sort w::binary-constraint)  (W::ATYPE (? ATYPE w::pre W::POST)))
   (ARGUMENTS
    (ARGUMENT (% (? ag W::S)) ONT::OF)
    (subcat (% w::adjp (w::set-modifier -)) ONT::result-VAL)
    ))
   
  ;; This is: this place is good for fishing 
  ;; using this template will require a subcat to be present
  ;; For "the place is good" use central-adj-templ in addition to this
  (adj-Purpose-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::Purpose)
    ))

  ;; This is: this place is good for fishing 
  ;; using this template will require a subcat to be present
  ;; For "the place is good" use central-adj-templ in addition to this
  (adj-affected-xp-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::Affected)
    ))

  
   (adj-affected-stimulus-xp-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::Affected)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::with)))) ONT::stimulus)
    ))

  ;; This is: this drug is good for cancer
  ;; using this template will require a subcat to be present
  ;; For "the place is good" use central-adj-templ in addition to this
  (adj-Purpose-implicit-xp-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::Purpose-implicit)
    ))
  
  
  ;; This template is for "I am afraid fo dogs"
  ;; Note that the complement is required. You need a "central-adj-templ" to go with this to have adjectives w/o complements
  ;; This is needed, however, for all ajectives which can have multiple complement mappings
  (adj-stimulus-xp-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::of)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::of)))) ONT::Stimulus)
    ))

  
  ;; This template is for "I am happy for her" Note that the
  ;; complement is required. You need a "central-adj-templ" to go with
  ;; this to have adjectives w/o complements This is needed, however,
  ;; for all ajectives which can have multiple complement mappings
  (adj-theme-xp-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::of)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::for)))) ont::formal)
    ))
  
  
  ;; This template is for "I happy that she does it"
  ;; Note that the complement is required. You need a "central-adj-templ" to go with this to have adjectives w/o complements
  ;; This is needed, however, for all ajectives which can have multiple complement mappings
  (adj-of-content-xp-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::of)
    (subcat (:parameter xp (:default (% W::CP (W::ctype W::s-that)))) ONT::Content)
    ))
  
  
  ;; For some adjectives content goes first: "to do this is hard"
  (adj-content-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::CONTENT)
    ))

  
  
  ;; This template is for "It's hard to see him"
  ;; Note that the complement is required. You need a "central-adj-templ" to go with this to have adjectives w/o complements
  ;; This is needed, however, for all ajectives which can have multiple complement mappings
  ;; The tricky bit here is that really we need an expletive "it" in this case
  ;; Currently the template is not perfect because to parse "be" sentences "it" is not allowed to be expletive
  ;; So we are forced into a compromise by requiring "it" but not requiring expletive (which would be an extra (w::sem ($ -)) on ARGUMENT)
  (adj-expletive-content-xp-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS    
    (ARGUMENT (% W::NP (W::lex W::it)) NOROLE)
    (subcat (:parameter xp (:default (% W::CP (W::ctype W::s-that)))) ONT::Content)
    ))

  
  ;; This template is for "I happy that she does it"
  ;; Note that the complement is required. You need a "central-adj-templ" to go with this to have adjectives w/o complements
  ;; This is needed, however, for all ajectives which can have multiple complement mappings
  (adj-content-affected-xp-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::Content)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::Affected)
    ))
  
  ;; a version of the above with an optional affected
  (adj-content-affected-optional-xp-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::Content)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::Affected)
    ))

  ;; This template is for "It's hard for him to see her"
  ;; Note that the complement is required. You need a "central-adj-templ" to go with this to have adjectives w/o complements
  ;; This is needed, however, for all ajectives which can have multiple complement mappings
  ;; The tricky bit here is that really we need an expletive "it" in this case
  ;; Currently the template is not perfect because to parse "be" sentences "it" is not allowed to be expletive
  ;; So we are forced into a compromise by requiring "it" but not requiring expletive (which would be an extra (w::sem ($ -)) on ARGUMENT)
  (adj-expletive-content-control-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP -))
   (ARGUMENTS    
    (ARGUMENT (% W::NP (W::lex W::it)) NOROLE)
    (subcat (:parameter xp1 
			(:default (% W::pp (W::ptype W::for))) 
			(:required (w::var ?subcatvar) (w::sem ?subcatsem) (w::lex ?subcatlex))) ONT::Affected)
    (subcat2 (:parameter xp2 
			 (:default (% W::CP (W::ctype W::s-to)))  
			 (:required (W::subj (% W::np (W::sem ?subcatsem) (W::lex ?subcatlex) (W::var ?subcatvar)))))
	     ONT::Content)
    ))
  

  (ADJ-property-TEMPL
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FORMAL)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::in)))) ONT::property)
    ))
  
  ;;;;; This is not fully implemented yet - intended for things like "5
  ;;;;; miles long", "premod" indicates a pre-modifying argument for
  ;;;;; adjectives
  (adj-premod-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::PRE) (W::ARG ?arg) (w::atype w::central))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (PREMOD (% W::NP) ONT::VAL)
    ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;; Bob's adjective templates
  ;;;;;
  ;;;;; these are both predicative and attributive, but not numerical,
  ;;;;; and take no compliments
  ;;;;; REPLACES ADJ-PRE-POST-TEMPL
  (central-adj-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::central) (W::SUBCAT -) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    ))

   (pre-adj-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::pre) (W::SUBCAT -) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    ))


   ;; such a problem
  (central-adj-sing-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::central) (W::SUBCAT -) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP (w::agr w::3s)) ONT::OF)
    ))


  ;; a happy person / the person is happy
  (central-adj-experiencer-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::central) (W::SUBCAT -) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ont::affected)
    ))

  ;; a sad movie/ the movie is sad
   (central-adj-content-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::central) (W::SUBCAT -) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::content)
    ))
  
  (central-adj-plur-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::central) (W::SUBCAT -) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP (w::agr w::3p)) ONT::OF)
    ))

   ;; allows an optional subcat  
   (central-adj-xp-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::central) (W::ALLOW-DELETED-COMP +) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::cp (W::ctype W::s-to)))) ONT::VAL)
    ))

    ;; optional subcat that can also be a pre modifier
    (central-adj-xp-possible-pre-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::central) (W::ALLOW-DELETED-COMP +) (W::ALLOW-PRE-MOD +) (W::ARG ?arg))
      (ARGUMENTS
        (ARGUMENT (% W::NP) ONT::OF)
        (subcat (:parameter xp (:default (% W::cp (W::ctype W::s-to)))) ONT::VAL)
    ))

    ;; requires the subcat  
   (central-adj-xp-required-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::central) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::cp (W::ctype W::s-to)))) ONT::VAL)
    ))
  
  ;;;;; Both predicative and atributive, and can take
  ;;;;; an optional complement in predicate position
  (central-adj-optional-xp-templ
   (SYNTAX (W::SORT W::PRED) (W::atype w::central) (W::ARG ?arg) (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::to)))) ONT::VAL) ;; MD: you should not define "optional" and "allow-deleted-comp" in the same template, or -adj-subcat> and -adj-pred-object-deleted> create an ambiguity
    ))
 
  ;;;;; attributive only adjectives, like "mere" or "former" which
  ;;;;; cant' go after the noun. (none of these can take compliments)
  ;;;;; no complement -- replaces simple-adj-templ
  (attributive-only-adj-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::attributive-only) 
	  (W::SUBCAT -) (W::ARG ?arg)
	  (W::ALLOW-DELETED-COMP +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    ))
  
  ;;;;; attributive only adjectives, like "mere" or "former" which
  ;;;;; cant' go after the noun. (none of these can take compliments)
  ;;;;; no complement -- replaces simple-adj-templ
  (attributive-only-adj-theme-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) 
	  (W::ATYPE W::attributive-only) (W::SUBCAT -) 
	  (W::ARG ?arg)  (W::ALLOW-DELETED-COMP +)
	  )
   (ARGUMENTS
    (ARGUMENT (% W::NP) ont::formal)
    ))
  
  (adj-theme-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::SUBCAT -) (W::ARG ?arg) (W::ATYPE W::central))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    ))
  
  (adj-experiencer-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::SUBCAT -) (W::ARG ?arg) (W::ATYPE W::central))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::AFFECTED)
    ))

   (adj-experiencer-content-xp-templ
   (SYNTAX (W::SORT W::PRED) (W::ARG ?arg) (W::ATYPE W::central) (w::allow-deleted-comp +))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::AFFECTED)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::of)))) ONT::content)
    ))

;; the task is easy to perform/ the car is easy to fix
  (adj-theme-action-templ
   (SYNTAX (W::SORT W::PRED) (W::ATYPE W::PREDICATIVE-only) (W::ARG ?arg)
	   (W::ALLOW-DELETED-COMP +)
	  )
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FORMAL)
    (subcat (:parameter xp (:default (% W::cp (W::ctype W::s-to)))) ONT::ACTION)
    ))
  
  ;;;;; predicative only, with optional complement like "afloat, afloat on the ocean"
  (predicative-adj-optional-xp-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::PREDICATIVE-only) (W::ALLOW-DELETED-COMP +) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::to)))) ONT::val optional)
    ))
  
  ;;;;; predicative only. Requires a complement, like "subject to" "tantamount to"
  (predicative-adj-req-xp-templ
   (SYNTAX(W::SORT W::PRED) (W::ATYPE W::PREDICATIVE-only) (W::ALLOW-DELETED-COMP -) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::to)))) ONT::val)
    ))
  
  ;;;;; predicative only. No complement allowed. Ex: "ablaze"
  (predicative-only-adj-templ
   (SYNTAX(W::SUBCAT -) (W::SORT W::PRED) (W::ATYPE W::PREDICATIVE-only) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    ))

  (predicative-only-experiencer-adj-templ
   (SYNTAX(W::SUBCAT -) (W::SORT W::PRED) (W::ATYPE W::PREDICATIVE-only) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ont::affected)
    ))
  
  ;;;;; These are adjectives that can be used post-positively
  ;;;;; (that is, after the noun without a verb, like "Christ almighty"
  ;;;;; if word is postpostive and takes no subcats.
  (postpositive-adj-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::postpositive) (W::SUBCAT -) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    ))
  
  ;;;;; These are adjectives that can be used post-positively
  ;;;;; (that is, after the noun without a verb, like "Christ almighty"
  ;;;;; if word is postpostive and takes no subcats.
  (postpositive-adj-theme-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ATYPE W::postpositive) (W::SUBCAT -) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::FORMAL)
    ))
   
  ;;;;; if word is postpositive and takes optional subcats
  ;;;;; "money enough" "money enough for all"
  (postpositive-adj-optional-xp-templ
   (SYNTAX(W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ALLOW-DELETED-COMP +) 
	  (W::ATYPE W::postpositive) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::to)))) ONT::val optional)
    ))

  ;; quiet enough for all
  (postpositive-adv-optional-xp-templ
   (SYNTAX (W::SORT W::PRED) (W::ALLOW-DELETED-COMP +) (W::ATYPE W::postpositive) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% (? W::argcat W::ADVBL W::ADJP)  (w::set-modifier -) (W::sort ?sort)) ONT::OF)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::for)))) ONT::val optional)
    ))
  
;   (binary-constraint-S-or-NP-decl-templ
;   (SYNTAX(W::SORT W::BINARY-CONSTRAINT) (W::ATYPE (? ATYPE W::PRE W::POST)))
;   (ARGUMENTS
;    (ARGUMENT (% (? ag W::S w::NP)) ONT::OF)
;    (SUBCAT (% W::S (W::stype (? st W::decl w::ing))) ONT::VAL)  
;    ))

   ;; the device placed at that time
  (binary-constraint-adj-postpos-templ
   (SYNTAX (W::SORT W::binary-constraint) (W::ATYPE W::postpositive) (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% (? W::argcat W::ADJP)  (w::set-modifier -) (W::sort ?sort)) ONT::OF)
    (subcat (% (? ag w::NP)) ONT::val)
    ))
  
  ;;;;; if word is postpositive and takes optional subcats
  ;;;;; "money enough" "money enough for all"
  (postpositive-adj-experiencer-theme-templ
   (SYNTAX (W::COMP-OP W::MORE) (W::SORT W::PRED) (W::ALLOW-DELETED-COMP +) (W::ATYPE W::postpositive) 
	   (W::ARG ?arg))
   (ARGUMENTS
    (ARGUMENT (% W::NP) ont::affected)
    (subcat (:parameter xp (:default (% W::pp (W::ptype W::to)))) ont::formal optional)
    ))
  
  ;;;;;; ;; Central adjs that can be used in one numerical construction
  ;;;;;; ;; NP1 is n units more ADJ than NP2
  ;;;;;; ;; (ex. My dog is 10 pounds heavier than your dog)
  ;;;;;; (numerical1-adj-templ
  ;;;;;; (Syntax
  ;;;;;; (COMP-OP more)
  ;;;;;; (SORT pred)
  ;;;;;; (ATYPE central)
  ;;;;;; (NUMERICAL-FORM numerical-both)
  ;;;;;; (Subcat -)
  ;;;;;; (arg ?arg)
  ;;;;;;)
  ;;;;;; (ARGUMENTS
  ;;;;;; (ARGUMENT (% NP) LF_OF)
  ;;;;;;)
  ;;;;;;)
  ;;;;;;
  ;;;;;; ;; Central adjs that can be used in both numerical constructions
  ;;;;;; ;; NP1 is n units more ADJ than NP2
  ;;;;;; ;; NP1 is n units ADJ
  ;;;;;; (numerical2-adj-templ
  ;;;;;; (Syntax
  ;;;;;; (COMP-OP more)
  ;;;;;; (SORT pred)
  ;;;;;; (ATYPE central)
  ;;;;;; (NUMERICAL-FORM numerical-comp)
  ;;;;;; (Subcat -)
  ;;;;;; (arg ?arg)
  ;;;;;;
  ;;;;;; (ARGUMENTS
  ;;;;;; (ARGUMENT (% NP) LF_OF)
  ;;;;;;)
  ;;;;;;)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;; Pronoun templates
  ;;;;;
  (pronoun-templ
   (SYNTAX(W::pro +) (W::MASS W::count) (W::status W::PRO) (W::case (? case W::sub W::obj -)) (W::agr 
      W::3s))
   (ARGUMENTS
    ))

  ;; mine, yours, his, hers, ours, theirs -- stand alone, don't modify a noun
  (poss-pronoun-templ
   (SYNTAX(W::pro +) (W::case W::poss) (W::status W::PRO) (W::poss +) (W::agr (? a w::3p W::3s)))
   (ARGUMENTS
    ))

  ;; my, your, his, her, its, our, their -- must modify a noun
  (poss-pro-det-templ
   (SYNTAX (W::pro +) (W::case W::poss) (W::status W::PRO-DET) (W::poss +) (W::agr (? a w::3p W::3s)))
   (ARGUMENTS
    ))

  ;; whose
  (poss-pro-det-indef-templ
   (SYNTAX (W::pro w::indef) (W::case W::poss) (W::status W::PRO-DET) (W::poss +) (W::agr (? a w::3p W::3s)))
   (ARGUMENTS
    ))
  
(pronoun-indef-templ
   (SYNTAX (W::MASS W::count) (W::status W::PRO) (W::case (? case W::sub W::obj -)) (W::PRO W::INDEF)
	   (W::agr W::3s))
   (ARGUMENTS
    ))

  (pronoun-plural-templ
   (SYNTAX (W::MASS W::count) (W::status W::PRO-SET) (W::case (? case W::sub W::obj -))
      (W::agr W::3P))
   (ARGUMENTS
    ))

  ;; each other, one another
  (pronoun-reciprocal-templ
   (SYNTAX(W::MASS W::count) (W::status W::PRO)  (W::case (? case W::obj -)) (W::PRO W::RECIP)
      (W::agr  (? a W::3s w::3p)))
   (ARGUMENTS
    ))

  (pronoun-quan-templ
   (SYNTAX(W::MASS W::count) (W::status W::QUANTIFIER) (W::case (? case W::sub W::obj -)) (W::PRO W::INDEF
     ) (W::agr W::3s))
   (ARGUMENTS
    ))
  
  (pronoun-wh-templ
   (SYNTAX(W::MASS W::count) (W::status W::WH) (W::case (? case W::sub W::obj -)) (W::PRO W::INDEF) 
     (W::agr W::3s))
   (ARGUMENTS
    ))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;
  ;;;;; Templates for sort pp-word, either nouns or adverbs
  ;;;;;
  (ppword-adv-templ
   (SYNTAX  (W::wh -) (W::sort W::pp-word) (W::atype (? atype W::pre W::post)))
   (ARGUMENTS
    (argument (:parameter xp (:default (% (? W::argcat W::S W::NP W::VP) (w::lex ?arglex)))) ONT::OF)
    ;;;;; Myrosia uncommented. Because the subcat is implicit, and we only need it to provide a selectional restriction
    ;;;;; Why do we have such a general subcat for these PP Adverbs? JFA 12/02
;    (subcat (% ?sc) ONT::VAL)
    ))

  
  (ppword-question-adv-templ
   (SYNTAX (W::sing-lf-only +) (w::else-word +) (W::wh W::q) (W::sort W::pp-word) (W::atype (? atype W::pre W::post)))
   (ARGUMENTS
    (argument (% W::S) ONT::OF)
        ;;; Subcat of PP words is implicit, and we need it to provide a selectional restriction, but it won't be matched with anything syntactically,
    ;;; hence is a very general category
;    (subcat (% ?sc) ONT::VAL)
    ))

  (ppword-question-adv-pred-templ
   (SYNTAX (W::sing-lf-only +) (w::else-word +) (W::wh W::q) (W::sort W::pp-word) (W::atype (? atype W::pre W::post)))
   (ARGUMENTS
    ;; For "where" and "when" we need argument to be either "S" for "where did this happen" or "NP" for "Where is he", because "where" in the latter
    ;; case is a predicate applying to a NP
    (argument (% (? argcat W::S W::NP) (w::lex ?arglex)) ONT::OF)
           ;;; Subcat of PP words is implicit, and we need it to provide a selectional restriction, but it won't be matched with anything syntactically,
    ;;; hence is a very general category
;    (subcat (% ?sc) ONT::VAL)
    ))

  (ppword-question-adv-how-templ
   (SYNTAX (W::sing-lf-only +) (W::wh W::q) (W::sort W::pp-word) (W::atype (? atype W::pre)))
   (ARGUMENTS
    (argument (% (? argcat W::ADJP W::ADVBL)  (w::set-modifier -) (w::lex ?arglex)) ONT::OF)
;    (subcat (% ?sc) ONT::VAL)
    ))
  
  (ppword-n-templ
   (SYNTAX (W::case (? case W::sub W::obj)) (W::agr W::3s) (W::sort W::pp-word) (W::MASS W::BARE))
   (ARGUMENTS
    ))
  
  (value-templ
   (SYNTAX(W::agr W::3s))
   (ARGUMENTS
    ))
  
  (name-templ
   (SYNTAX(W::agr W::3s) (W::name +) (W::generated -))
   (ARGUMENTS
    ))

   (nname-templ   ;; template for special names that can be used as modifiers in constructions like "delta 567"
    (SYNTAX (W::agr W::3s) (W::name +) (W::nname +) (W::generated -))
    (ARGUMENTS
     ))
  
  (fp-templ
    ;;(syntax (w::skip +))      ;; grammar already inserts this when the FP is treated as a PAUSE
    (ARGUMENTS
    ))
  
  (ordinal-templ
   (syntax (W::morph (:forms (-S-3P))))  ;; allow plurals: two-thirds
   (ARGUMENTS
    ))
  
  (no-features-templ
   (ARGUMENTS
    ))
  
  ;;;;; this is used for 'a.m.' and 'p.m.' in values.lisp
  (pred-no-form-templ
   (SYNTAX(W::SORT W::PRED) (W::AGR W::3s) (W::CASE (? cas W::sub W::obj)) (W::morph (:forms (-none))) (
     W::MASS W::COUNT))
   (ARGUMENTS
    ))
))