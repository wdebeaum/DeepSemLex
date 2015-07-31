(in-package :om)
;; This is VN masquerade-0.26
;; act, behave
(define-type ont::acting
 :wordnet-sense-keys ("do%2:29:09" "behave%2:29:00" "act%2:29:00" "act%1:03:00" "deed%1:03:00" "human_action%1:03:00" "human_activity%1:03:00")
    :parent ont::event-of-action
    :arguments ((:required ont::agent  ((? cz F::Phys-obj f::abstr-obj f::situation)))

;		(:essential ont::predicate)
;		(:optional ont::formal) ;; he acted the part
;		(:optional ont::agent  ((? aoc F::phys-obj F::abstr-obj) (F::intentional +)) (:implements cause))
		))

;; 04262012 adding (really, reinstating) this for gloss to get better readings of the definitions
;; :origin jr gloss-variant
(define-type ont::act
    :wordnet-sense-keys ("atone%2:37:00" "break_even%2:40:00" "campaign%2:33:00" "err%2:31:00" "excel%2:42:00" "exercise%2:29:00" "experiment%2:41:02" "fall%2:41:15" "fall_in%2:41:02" "go%2:33:00" "hide%2:39:01" "indulge%2:41:01" "keep_one's_mouth_shut%2:32:00" "keep_to_oneself%2:41:00" "keep_up%2:33:00")
    :parent ont::event-of-action
    :arguments ((:optional ont::agent ((? cz F::Phys-obj f::abstr-obj f::situation)))
		(:required ont::of  ((? of f::situation)))
		))

(define-type ONT::cause-effect
;; 20121101 gloss change removing incorrect "produce%2:36:03" "bring_about%2:36:03" "give_rise%2:36:01" synset mapping 
 :wordnet-sense-keys ( "cause%2:36:00" "do%2:36:02" "make%2:36:08" ;;"produce%2:36:03" "bring_about%2:36:03" "give_rise%2:36:01"
  "receive%2:35:00" "drive%2:35:00" "get%2:30:02" "get%2:30:00" )
 :parent ONT::acting
 :sem (F::Situation (F::Cause (? cz F::Force f::agentive)) (F::Trajectory -))
 :arguments ((:ESSENTIAL ONT::agent((? oc F::Phys-obj F::Abstr-obj F::Situation)))
	     (:ESSENTIAL ONT::AFFECTED)
	     (:ESSENTIAL ONT::formal ((? eoc F::situation F::Abstr-obj)))
	     (:optional ont::result)
	     )
 )

(define-type ONT::inhibit-effect
 :parent ONT::acting
 :sem (F::Situation (F::Cause (? cz F::Force f::agentive)) (F::Trajectory -))
 :arguments ((:ESSENTIAL ONT::affected ((? oc F::Phys-obj F::Abstr-obj F::Situation)))   ;; leave in F::Situation, e.g., inhibits expression, inhibits growth
             (:ESSENTIAL ONT::formal ((? eoc2 F::situation)));; F::Abstr-obj f::phys-obj f::time)))
             (:OPTIONAL ONT::agent ((? aoc F::phys-obj F::abstr-obj) (F::intentional +)) (:implements cause))
             )
 )

(define-type ONT::agent-interaction
 :parent ONT::event-of-agent-interaction
 :arguments (;;; Communicator
             (:ESSENTIAL ONT::Agent ((? atp F::phys-obj F::abstr-obj) (F::intentional +)))
             )
 )

(define-type ONT::Communication
 :wordnet-sense-keys ("put_across%2:32:00" "pass_along%2:32:00" "pass%2:32:01" "pass_on%2:32:00" "communicate%2:32:01" "intercommunicate%2:32:00" "communicate%2:32:00" "communication%1:03:00")
 :parent ONT::agent-interaction
 :sem (F::Situation (F::Cause F::Force) (F::Trajectory -))
 :arguments (;;; Addressee
             ;;(:ESSENTIAL ONT::Addressee ((? adr F::Phys-obj f::abstr-obj) (F::intentional +)))
	     (:ESSENTIAL ONT::Affected ((? adr F::Phys-obj f::abstr-obj)))
             ;;; Message -- the information
	     ;; swift 04/28/04
	     ;; this feature restriction prohibits utts like "show me the computer" --either this should be restricted at a lower
	     ;; level or some of the child lf-types like ont::show should be moved.
	     (:ESSENTIAL ONT::Formal (F::situation)) ;;?type (F::information F::information-content)))
	                  ;;; Medium
             (:OPTIONAL ONT::Instrument ((? inst F::phys-obj f::abstr-obj) (F::intentional -)))
             ;;; topic, about smth - has an arbitrary restriction
             (:OPTIONAL ONT::Associated-information) ;; they talked ABOUT X
             )
 )

(define-type ONT::agreement
 :parent ONT::agent-interaction
 :arguments ((:ESSENTIAL ONT::Formal)
             (:OPTIONAL ONT::Associated-information)
	     )
 )

;; here we don't require the formal to be a movable physical object
;; so we can say e.g. "put the title in the text box" and "put the city here"
;; the unrestricted formal allows us to get rid of the text-representation rule in phrase-grammar
(define-type ont::motion
 :parent ONT::EVENT-OF-causation
 :sem (F::Situation (F::Cause (? c F::Force -)) (F::Aspect F::Dynamic))
 :arguments ((:REQUIRED ONT::affected ((? th1 f::phys-obj f::abstr-obj f::situation f::time))) ;; mostly unrestricted
             ;;; Source Path Goal Area
	     (:OPTIONAL ONT::Instrument (F::Phys-obj (F::mobility F::movable) (F::intentional -)))
             ;;; Now the list of "trajectory" roles
             (:OPTIONAL ONT::Source )
             (:OPTIONAL ont::result )
             ;;(:OPTIONAL ONT::From-Loc (F::Phys-obj (F::spatial-abstraction F::Any-spatial-abstraction))
             ;; (:implements source))
	     ;;(:OPTIONAL ONT::To-Loc (F::Phys-obj (F::spatial-abstraction F::Any-spatial-abstraction))
             ;; (:implements goal))
	     (:OPTIONAL ONT::agent ((? causetype F::phys-obj F::situation F::abstr-obj)))
	     )
 )

;; must be a movable, physical object
(define-type ont::physical-motion
 :parent ONT::motion
 :sem (F::Situation (F::Cause (? c F::Force -)) (F::Aspect F::Dynamic))
 :arguments ((:REQUIRED ONT::affected (F::Phys-obj (F::mobility F::movable)))
	     (:optional ont::extent (F::abstr-obj (F::type ont::quantity))
             )
 ))

;; perceive using senses
(define-type ONT::Perception
 :wordnet-sense-keys ("feel%2:35:00")
 :parent ONT::event-of-action
 ;;; Perceiver
 :arguments ((:ESSENTIAL ONT::agent (F::Phys-obj (F::origin (? o F::human f::non-human-animal))))
             ;;; Phenomenon
             (:ESSENTIAL ONT::neutral)
	     (:optional ONT::neutral1)
	     (:optional ont::formal)
             )
 )

;;; ONT::INTERACT has moved to specific-situation-types

(define-type ONT::Objective-influence
    :wordnet-sense-keys ("force%1:07:01" "appeal%2:37:00" "keep_up%2:29:00" "assail%2:33:00" "retire%2:33:00" "sensitise%2:39:00" "sound%2:39:01" "stampede%2:38:01" "stampede%2:41:01" "trip%2:38:01" "unbalance%2:42:00" "undo%2:36:00")
    :parent ONT::EVENT-OF-causation
    :sem (F::Situation (F::Trajectory -))
 :arguments ( ;; e.g. "plans" in "it affects his plans"
	     (:REQUIRED ONT::Affected ((? o1 F::Situation F::Phys-obj f::abstr-obj)))
             ;; e.g. switch in "the switch affects the bulb"
             (:ESSENTIAL ONT::agent ((? o2 F::Situation F::Phys-obj f::abstr-obj)))
             ;;; result of interaction
             (:OPTIONAL ONT::Result (F::Phys-obj))
	     (:OPTIONAL ONT::Effect (F::Situation)) 
             )
 )

(define-type ONT::Body-movement
 :wordnet-sense-keys ("change_posture%2:38:00")
 :parent ONT::EVENT-OF-causation
 :sem (F::Situation (f::trajectory +))
 ;;; Agent
 :arguments (;;(:ESSENTIAL ONT::Agent (F::phys-obj (F::intentional +)))
             ;;; BodyPart
             (:ESSENTIAL ONT::affected)
             ;;; Cause/Internal Cause
	     (:ESSENTIAL ONT::agent ((? o2 F::Situation F::Phys-obj f::abstr-obj)))
              ;;; Addressee (e.g. wink at/wave to someone)
             (:OPTIONAL ONT::Addressee)
             ;;; Path/Goal/Area/Source
             (:OPTIONAL ONT::Path)
             )
 )

;; hold/grasp something
(define-type ONT::Body-manipulation
 :wordnet-sense-keys ("hold%2:35:00" "take_hold%2:35:00" "bear%2:35:01" "immobilise%2:35:00")
 :parent ONT::EVENT-OF-causation
 :sem (F::Situation (F::Cause F::Agentive))
 ;;; Agent
 :arguments ((:ESSENTIAL ONT::Agent (F::phys-obj (F::intentional +)))
             ;;; Entity
             (:REQUIRED ONT::affected (F::phys-obj (F::intentional -)))
             ;;; Locus
             (:OPTIONAL ONT::Place)
             ;;; Agent's body part
             (:OPTIONAL ONT::Instrument (F::phys-obj (f::origin f::living) (F::intentional -)))
             )
 )

;; cognizer has some mental attitude towards some condition, state or situation
;; awareness, consciousness, know
(define-type ONT::Awareness
 :wordnet-sense-keys ("think%2:31:00" "cogitate%2:31:00" "cerebrate%2:31:00")
 :parent ONT::event-of-state
 :sem (F::Situation (F::Cause F::Mental) (F::Trajectory -))
 :arguments ((:ESSENTIAL ONT::Formal)
	     (:OPTIONAL ont::neutral  ((? cg2 f::abstr-obj F::Phys-obj) (F::intentional +)))
	     (:OPTIONAL ont::neutral1  ((? cg1 f::abstr-obj F::Phys-obj)))
             )
 )


(define-type ONT::Change-Awareness
 :parent ONT::event-of-awareness
 :sem (F::Situation (F::Cause F::Mental) (F::Trajectory -))
 :arguments ((:REQUIRED ONT::agent ((? cg f::abstr-obj F::Phys-obj) (F::intentional +)))
              (:ESSENTIAL ONT::Formal)
              (:OPTIONAL ONT::Source)
             )
 )


(define-type ONT::Categorization
 :wordnet-sense-keys ("declare%2:32:04" "adjudge%2:32:00" "hold%2:32:11" "see%2:31:00")
 :parent ONT::event-of-action
 :sem (F::Situation (F::Cause F::Agentive (F::trajectory -)))
 :arguments ((:ESSENTIAL ONT::Agent ((? cog f::abstr-obj F::phys-obj) (F::intentional +)))
             ;;; Item
             (:REQUIRED ONT::neutral ((? th2 F::Phys-obj F::Abstr-obj F::situation)))
             ;;; Category
	     (:OPTIONAL ONT::formal ((? prd F::Phys-obj F::Abstr-obj F::situation))) ;;classify this as a mine, call me ishmael
           ;  (:OPTIONAL ONT::Criterion)
	     (:optional ont::formal1 (?cth (f::intentional -)) (:implements criterion))
             
             )
 )


(define-type ONT::Cogitation
 :wordnet-sense-keys ("look_at%2:31:00" "deal%2:31:10" "take%2:31:03" "consider%2:31:01" "cogitate%2:31:01" "bethink%2:39:00" "brood%2:42:00" "consider%2:32:00")
 :parent ONT::change-awareness
 :sem (F::Situation (:required (F::Trajectory -)))
 :arguments ((:ESSENTIAL ONT::Neutral ((? atp F::phys-obj F::abstr-obj F::situation)))
             (:ESSENTIAL ONT::Formal ((? th3 F::Phys-obj F::Abstr-obj F::situation)))
	     (:optional ont::predicate)
             )
 )


;(define-type ONT::mental-action
; :parent ONT::event-of-awareness
; :sem (F::situation (:required (F::cause F::agentive) (F::trajectory -))(:default (F::aspect F::unbounded) (
;                  F::time-span F::atomic)))
; :arguments ((:ESSENTIAL ONT::agent  ((? agt F::Phys-obj f::abstr-obj) (F::intentional +)))
;             (:ESSENTIAL ONT::Formal  ((? th F::Abstr-obj F::situation)))
;             )
; )


;; FN coming-to-believe
;; infer, figure out, work out, guess, ascertain
;; cognizer reasoning results in a conclusion
;; compare related type ont::becoming-aware which relies more on straight perception
(define-type ONT::Coming-to-Believe
 :wordnet-sense-keys ("reason%2:31:01" "reason_out%2:31:00" "conclude%2:31:00" "ascertain%2:32:00" "ascertain%2:32:01" "discover%2:31:01")
 :parent ONT::cogitation
 :arguments ((:ESSENTIAL ONT::Agent  ((? agt F::Phys-obj f::abstr-obj) (F::intentional +)))
             ;;; Evidence
             (:OPTIONAL ONT::Source)
             ;;; Content/Topic
             (:REQUIRED ONT::Formal((? thm F::Abstr-obj F::situation)))
             )
 )

;; cognizer expects a situation to hold
(define-type ONT::expectation
 :wordnet-sense-keys ("expect%2:31:00" "anticipate%2:31:00")
 :parent ONT::awareness
 :sem (F::SITUATION (F::Aspect F::static) (F::Time-span F::extended) (F::Trajectory -))
  :arguments ((:ESSENTIAL ONT::neutral (F::phys-obj (F::intentional +)))
	     (:OPTIONAL ONT::Formal)
             (:OPTIONAL ONT::Action (F::Situation))
             )
 )


;; cf related concept ont::create, with agent and formal roles
(define-type ONT::Invention
 :parent ONT::event-of-creation
 :arguments ((:ESSENTIAL ONT::agent (F::phys-obj (F::intentional +)) )
             ;;; Invention
	     ;; ONT::result is our name for VN product
	     (:optional ONT::affected-Result)
             (:OPTIONAL ONT::Purpose)
	     )
 )

(define-type ONT::Salience
 :parent ONT::event-of-state
 :sem (F::Situation (F::Cause F::Mental) (F::Trajectory -))
 ;;; Content
 :arguments ((:REQUIRED ONT::Formal)
             (:OPTIONAL ONT::neutral ((? atp F::phys-obj F::abstr-obj) (F::intentional +)))
             ;;; Evidence
             (:OPTIONAL ONT::Source)
             ;;; Ground/ Loc-Perc
             (:OPTIONAL ONT::Place)
             )
 )

;; cognizer acquires a believe about a situation
;; learn
(define-type ONT::acquire-belief
 :parent ONT::cogitation
 :arguments ((:REQUIRED ONT::Formal ((? th4 f::phys-obj f::abstr-obj f::situation))) ;;Ground
             (:ESSENTIAL ONT::Agent  ((? agt F::Phys-obj f::abstr-obj) (F::intentional +)))
	     (:optional ont::effect (f::situation))
             )
 )

;; cognizer acquires a believe about a situation and it actually holds
;; get, grasp, realize, see, understand
(define-type ONT::come-to-understand
 :wordnet-sense-keys ("understand%2:31:00" "see%2:31:01" "realise%2:31:00" "realize%2:31:00" "understand%2:31:01")
 :parent ONT::acquire-belief
 :arguments ((:REQUIRED ONT::Formal ((? th5 f::phys-obj f::abstr-obj f::situation))) ;;Ground
             (:ESSENTIAL ONT::Agent  ((? agt F::Phys-obj f::abstr-obj) (F::intentional +)))
             )
 )

;; by means of perception
;; wonder, discover, find-out
(define-type ONT::Becoming-Aware
 :wordnet-sense-keys ("perceive%2:31:00" "perceive%2:39:00" "comprehend%2:39:00" "record%2:39:00" "mark%2:39:00" "nose_out%2:39:00")
 :parent ONT::acquire-belief
 :arguments (             ;;; Evidence
             (:OPTIONAL ONT::Source)
             ;;; State
             (:OPTIONAL ONT::Stative)
	     ;; why does this have both agent and cognizer roles (see above)? 
             (:ESSENTIAL ONT::agent () (:implements cognizer))
             )
 )

;; cognizer evaluates something
(define-type ONT::Scrutiny
    :wordnet-sense-keys ("take%2:31:02" "read%2:31:05" "study%2:31:00" "learn%2:31:02" "inspect%2:31:00" "scrutinise%2:31:00" "scrutinize%2:31:00" "audit%2:31:01" "inspect%2:39:00" "try%2:41:06" "adjudicate%2:41:03" "judge%2:41:09" "measure%2:31:01" "evaluate%2:31:00" "valuate%2:31:00" "assess%2:31:00" "appraise%2:31:01" "value%2:31:00" "analyse%2:31:00" "analyse%2:31:01" "analyse%2:31:04" "check%2:42:09" "check_out%2:40:00" "follow%2:30:01" "follow%2:39:13")
    :parent ONT::cogitation
    :arguments ((:REQUIRED ONT::neutral ((? th6 f::phys-obj f::abstr-obj f::situation))) ;;Ground
		(:OPTIONAL ONT::formal1 ((? cth f::phys-obj f::abstr-obj f::situation)))  ;;Phenomenon
		(:ESSENTIAL ONT::Agent ((? agt F::Phys-obj f::abstr-obj) (F::intentional +)))
		)
    )

(define-type ont::compare
    :parent ont::scrutiny
    :wordnet-sense-keys ("compare%2:31:00" "contrast%2:31:00")
    :arguments ((:REQUIRED ONT::neutral1 ((? th7 f::phys-obj f::abstr-obj f::situation))))
    )

;; cognizer reasoning results in a value
;; approximate, assess, estimate, fix, guage
(define-type ONT::Becoming-Aware-of-value
 :wordnet-sense-keys ("judge%2:31:00" "pass_judgment%2:31:00" "evaluate%2:31:01" "figure%2:31:00" "reckon%2:31:01" "work_out%2:31:06" "compute%2:31:00" "cypher%2:31:00" "cipher%2:31:00" "calculate%2:31:00" "process%2:31:00")
 :parent ONT::scrutiny
 ;;; Phenomenon
 :arguments ((:REQUIRED ONT::Formal (F::ABSTR-OBJ))
             )
 )

(define-type ONT::Conversation
 :wordnet-sense-keys ("talk_about%2:32:01" "talk_of%2:32:00" "converse%2:32:00" "discourse%2:32:01" "correspond%2:32:00")
 :parent ONT::event-type
 :sem (F::Situation (F::Cause F::Agentive))
 ;;; We inherit most of the arguments and add a formal1
 ;;; :arguments ((:OPTIONAL formal1))
 :arguments ((:ESSENTIAL ONT::agent ((? atp F::phys-obj F::abstr-obj) (F::intentional +)))
	     (:ESSENTIAL ONT::agent1 ((? atp F::phys-obj F::abstr-obj) (F::intentional +)))
	     (:OPTIONAL ONT::Associated-information) ;; they talked ABOUT X
             )
 )

(define-type ONT::encoding
 :wordnet-sense-keys ("spell%2:32:00" "spell_out%2:32:01" "spell%2:36:00")
 :parent ONT::communication
 ;;; All inherited arguments, plus manner and purpose
 :arguments ((:OPTIONAL ONT::Manner)
             (:OPTIONAL ONT::Purpose)
             )
 )

(define-type ONT::Questioning
 :wordnet-sense-keys ("ask%2:32:00" "inquire%2:32:01" "enquire%2:32:00")
 :parent ONT::COMMUNICATION
 :sem (F::Situation (F::Cause F::Agentive))
 :arguments ((:REQUIRED ONT::formal)
             (:OPTIONAL ONT::associated-information)
             )
 )

(define-type ONT::Request
 :wordnet-sense-keys ("quest%2:32:01" "call_for%2:32:04" "bespeak%2:32:00" "request%2:32:01" "request%1:10:00" "asking%1:10:00")
 :parent ONT::COMMUNICATION
 :sem (F::Situation (F::Cause F::Agentive))
 :arguments ((:ESSENTIAL ONT::effect ((? t f::situation)))
             )
 )

;; accept, reject, etc.
(define-type ONT::Response
 :wordnet-sense-keys ("react%2:31:00" "respond%2:31:00")
 :parent ONT::agreement
 :sem (F::Situation (F::Cause F::Agentive))
 :arguments ((:OPTIONAL ONT::affected ((? adr F::Phys-obj f::abstr-obj)))
             )
 )

;; express -- can be nonverbal.
(define-type ont::express
 :wordnet-sense-keys ("state%2:32:02" "express%2:32:03" "give_tongue_to%2:32:00" "utter%2:32:01" "verbalise%2:32:03" "verbalize%2:32:03" "express%2:32:00" "frame%2:32:00" "redact%2:32:00" "cast%2:32:00" "put%2:32:00" "couch%2:32:00")
  :parent ont::communication
  )

(define-type ONT::Statement
 :wordnet-sense-keys ("tell%2:32:04" "say%2:32:00" "state%2:32:00" "speak%2:32:01" "talk%2:32:01" "inform%2:32:00")
 :parent ONT::COMMUNICATION
 :sem (F::Situation (:default (F::Cause F::Agentive) (f::aspect f::dynamic)))
 ;;; you can talk about anything -- people, vehicles, situations, etc.
 ;;; (?type (information F_Information-content)))
 :arguments ((:ESSENTIAL ONT::formal)
             (:OPTIONAL ONT::associated-information)
	     (:optional ont::effect ((? ef F::Situation f::abstr-obj)))
             )
 )

(define-type ONT::announce
 :wordnet-sense-keys ("present%2:32:00" "represent%2:32:11" "lay_out%2:32:00")
 :parent ONT::statement
 )

(define-type ONT::offer
 :parent ONT::statement
 :arguments ((:REQUIRED ont::result)
             )
 )

;;; swift 10/27/06 changed f::origin f::human to f::origin f::living for e.g. "every dog hates a cat"; "plants love sunshine"
;;; I conjoined Experiencer-subj with Experiencer-obj
;;; swift 01/12/01 -- changed aspect feature f_static to F_Stage-Level to allow progressive
(define-type ONT::Experiencer-emotion
 :wordnet-sense-keys ("like%2:37:05" "experience%2:37:00" "feel%2:37:00" "feeling%1:03:00")
 :parent ONT::event-of-state
 :sem (F::Situation (:required (F::Cause F::Mental))(:default (F::Aspect F::Stage-Level)))
 :arguments ((:ESSENTIAL ONT::neutral ((? ep  F::Phys-obj f::abstr-obj) (F::intentional +)))
              (:OPTIONAL ONT::neutral1) 
             (:OPTIONAL ONT::Formal) 
             ;;; Means
             (:OPTIONAL ONT::action)
             )
 )

(define-type ONT::life-process
 :parent ONT::EVENT-OF-undergoing-action
 :arguments ((:OPTIONAL ONT::affected (F::phys-obj (F::origin F::living)))
             )
 )

(define-type ONT::Cure
 :wordnet-sense-keys ("bring_around%2:29:01" "cure%2:29:00" "heal%2:29:01" "care_for%2:29:00")
 :parent ONT::LIFE-PROCESS
 ;;; Healer
 :arguments ((:OPTIONAL ONT::Agent)
              (:OPTIONAL ONT::affected () (:implements experiencer))
             ;;; Affliction
             (:OPTIONAL ONT::Formal)
             ;;; Treatment
             (:OPTIONAL ONT::INSTRUMENT (F::phys-obj (F::intentional -)))
             )
 )


(define-type ONT::Cause-to-Move
 :wordnet-sense-keys ("reflect%2:39:00" "drive%2:35:01")
 :parent ont::physical-motion
 :sem (F::Situation (F::Cause F::Force) (f::trajectory +))
 :arguments ((:ESSENTIAL ONT::agent)
             )
 )

(define-type ONT::Co-motion
 :parent ont::physical-motion
 :arguments ((:essential ont::neutral)   ;; the object with which the motion is relative to
             )
 )

(define-type ONT::Emptying
 :parent ont::physical-motion
 )

(define-type ONT::Filling
 :wordnet-sense-keys ("impregnate%2:30:00" "saturate%2:30:04" "spread%2:35:13")
 :parent ont::physical-motion
 :arguments ((:ESSENTIAL ONT::affected-result)
             )
 )

;;; Even if we do not know the path, it is implied in the verb
(define-type ONT::Path-shape
 :parent ont::physical-motion
 :arguments ((:ESSENTIAL ONT::Path)
             )
 )

;; for verbs that commonly allow non-physical and/or non-moveable formals, e.g.
;; put the title in the text box, put the city here, insert the text here
(define-type ONT::put
 :wordnet-sense-keys ("put%2:35:00" "set%2:35:00" "place%2:35:00" "pose%2:35:02" "position%2:35:00" "lay%2:35:01" "interpose%2:38:01")
 :parent ont::motion
 :arguments ((:ESSENTIAL ONT::agent)
	     ;;(:OPTIONAL ONT::Spatial-Loc (F::Phys-obj (F::spatial-abstraction F::Any-spatial-abstraction))
             ;; (:implements goal))
             )
 )

;;; These are all cases where there is an (implicit) agent directing the motion
(define-type ONT::directed-motion
 :parent ont::physical-motion
 :arguments ((:ESSENTIAL ONT::agent)
             (:OPTIONAL ONT::assoc-with) ;; what's this for?
             )
 )

(define-type ONT::Placing
 :parent ONT::DIRECTED-MOTION
 )

(define-type ONT::Arriving
 :parent ont::physical-motion
 :arguments ((:ESSENTIAL ONT::To-loc)
             )
 )

(define-type ONT::Departing
 :parent ont::physical-motion
 :arguments ((:ESSENTIAL ONT::From-loc)
	     (:optional ont::neutral)
             )
 )

;; enter,  ingress
(define-type ONT::ENTERING
 :wordnet-sense-keys ("enter%2:38:00" "come_in%2:38:02" "get_into%2:38:00" "get_in%2:38:01" "go_into%2:38:00" "go_in%2:38:00" "move_into%2:38:00" "enter%2:36:00")
 :parent ont::physical-motion
 :arguments ((:REQUIRED ONT::affected ((? ttype f::phys-obj)))
	     (:ESSENTIAL ont::result (F::phys-obj (F::spatial-abstraction (? sa F::spatial-region))
					       (F::object-function (? of f::spatial-object f::building))))
	     (:optional ont::neutral)
	     )
 )

;; poke, prod, ...
(define-type ONT::Penetrate
 :wordnet-sense-keys ("poke%2:35:01" "stab%2:35:02" "penetrate%2:35:00")
 :parent ont::entering
 :arguments ((:REQUIRED ONT::agent ((? ttype f::phys-obj)))
	     (:ESSENTIAL ONT::affected (F::phys-obj (F::spatial-abstraction (? sa F::spatial-region))
					       (F::object-function (? of f::spatial-object f::building))))
	     )
 )

(define-type ONT::Transportation
 :parent ont::physical-motion
 :sem (F::Situation (F::Cause F::Agentive))
 :arguments ((:ESSENTIAL ONT::Instrument (F::phys-obj (F::object-function F::vehicle) (F::intentional -)))
             )
 )


(define-type ONT::storing
  :parent ONT::directed-motion
  :arguments ((:REQUIRED ONT::affected ((? ttype F::Abstr-obj f::phys-obj))) ;; storee
	      (:OPTIONAL ONT::agent (F::phys-obj (F::intentional +))) ;; storer
	      )
 )
(define-type ONT::Active-Perception
 :wordnet-sense-keys ("look%2:39:00" "feel%2:39:00" "sense%2:39:00" "note%2:39:02" "look_on%2:39:00" "look_out%2:39:00" "watch%2:39:00" "watch%2:39:03")
 :parent ONT::PERCEPTION
; :sem (F::Situation (F::trajectory +)) ;; can perceive along trajectories; on your left you see a building; you hear a noise to your right
 :arguments ((:ESSENTIAL ONT::agent (F::Phys-obj (F::Intentional +)))
             ;;; Phenomenon
             (:ESSENTIAL ONT::neutral ((? tt F::phys-obj F::situation)))
	     (:ESSENTIAL ONT::action ((? act F::situation))) ;; He saw him leave - present treatment 
	      (:optional ONT::formal)
             )
 )

(define-type ONT::Appearance
 :parent ONT::PERCEPTION
 )

(define-type ONT::Expensiveness
 :parent ONT::event-of-state
 :sem (F::Situation (F::Cause -) (F::trajectory -))
 :arguments ((:ESSENTIAL ONT::Cost ((? cst f::phys-obj f::abstr-obj))) ; too restrictive (f::abstr-obj (f::scale f::money-scale)))
	     ;;(:optional ont::effect (f::situation))
	     (:REQUIRED ONT::FORMAL ((? th8 F::Phys-obj F::Abstr-obj F::situation)))
             )
 )

(define-type ONT::activity
 :wordnet-sense-keys ("activity%1:04:00")
 :parent ONT::event-of-action
  :sem (F::Situation (:required (F::trajectory -))(:default (F::aspect F::dynamic)(F::time-span F::extended)))
 )

;;; general class for ingesting, eat, drink, take (drugs), ...
;;; NB: figure out about feed.
(define-type ONT::consume
 :wordnet-sense-keys ("consume%2:34:00" "ingest%2:34:00" "take_in%2:34:00" "take%2:34:00" "have%2:34:00")
 :parent ONT::event-of-causation
 :sem (F::Situation (:required (F::trajectory -))(:default (F::Cause F::agentive) (F::aspect F::dynamic) (F::time-span F::extended)))
 :arguments ((:REQUIRED ONT::Agent (F::Phys-obj (f::origin f::living)))
             ;;; Formal changed to comestible, although probably too strong
	     ;; form substance is also too strong. 
             (:REQUIRED ONT::Affected (F::Phys-obj (F::mobility F::movable) (F::form F::substance) (f::object-function f::comestible))) 
	     ;; This is eat with a spoon/fork etc.
             (:OPTIONAL ONT::Instrument (F::Phys-obj (F::mobility F::movable) (F::intentional -) (f::form f::solid-object) (f::information -)))
	     ;;; Myrosia added accompaniment to account for frequent usages of "take/eat it with food/milk etc
	     (:OPTIONAL ONT::Assoc-with (F::Phys-obj (F::Form F::substance) (F::mobility F::movable)))
             )
 )

(define-type ONT::AUX
 :parent ONT::SITUATION-ROOT
 :arguments ((:REQUIRED ONT::Formal)
             (:ESSENTIAL ONT::Situation (F::Situation))
             )
 )

;;; deontic modality can be handled as a state;
;;; epistemic modality can't be classified in terms of eventualities --
;;; this distinction isn't currently addressed
;;;
;;; can
(define-type ONT::ABILITY
 :parent ONT::AUX
 :sem (F::Situation (F::Aspect F::static) (F::Time-span F::Extended) (F::Trajectory -))
 )

;;; may, might
(define-type ONT::POSSIBILITY
 :parent ONT::AUX
 :sem (F::Situation (F::Aspect F::static) (F::Time-span F::Extended) (F::Trajectory -))
 )

;;; should -- used to be ont::obligation but label was at times inappropriate
(define-type ONT::should
 :parent ONT::AUX
 :sem (F::Situation (F::Aspect F::static) (F::Time-span F::Extended) (F::Trajectory -))
 )

;;; must -- used to be ont::obligation but label was at times inappropriate
(define-type ONT::must
 :parent ONT::AUX
 :sem (F::Situation (F::Aspect F::static) (F::Time-span F::Extended) (F::Trajectory -))
 )

;;; DO
;;; carries no Aspect or Time-span features of its own
(define-type ONT::DO
 :parent ONT::AUX
 :sem (F::situation)
 )

;;; WILL, SHALL
;;; carries no Aspect or Time-span features of its own
(define-type ONT::FUTURE
 :parent ONT::AUX
 :sem (F::situation)
 )

;;; WOULD, SHOULD
(define-type ONT::CONDITIONAL
 :parent ONT::AUX
 :sem (F::situation (F::Aspect F::static) (F::Time-span F::extended))
 )

;;; swift 02/12/01 -- add semantic restictions to application of the progressive rule. VPs with dynamic
;;; aspect or stage-level stative predicates (e.g. feeling good) can be progressive, but not individual
;;; level stative predicates (e.g. know the answer)
(define-type ONT::PROGRESSIVE
 :parent ONT::AUX
 :sem (F::Situation (F::Aspect F::static) (F::Time-span F::extended))
 :arguments ((:ESSENTIAL ONT::Situation (F::Situation (F::Aspect (? avar F::Dynamic F::Stage-Level))))
             )
 )

;;; for gonna and be going to
(define-type ONT::GOING-TO
 :parent ONT::AUX
 :sem (F::Situation (F::Aspect F::static) (F::Time-span F::extended))
 ;; Myrosia 2005/06/14 commented out the restriction on situation because we can also say
 ;; I am going to be at home/ He is going to see / He is going to know that ... - so there is no restriction on aspect here
 ;; :arguments ((:ESSENTIAL ONT::Situation (F::Situation (F::Aspect (? avar F::Dynamic F::Stage-Level))))
 ;;             )
 )

;;; carries no Aspect or Time-span features of its own
(define-type ONT::PASSIVE
 :parent ONT::AUX
 :sem (F::situation)
 )

(define-type ONT::PERFECTIVE
 :parent ONT::AUX
 :sem (F::Situation (F::Aspect F::static) (F::Time-span F::extended))
 :arguments ((:ESSENTIAL ONT::Situation (F::Situation (F::Aspect (? avar F::Dynamic F::Stage-Level))))
             )
 )

;; event nouns that are non-located -- e.g. have/experience a headache
;; 12/2010 -- conflating with ont::participating, so now allowing located event nouns
(define-type ONT::have-experience
 :wordnet-sense-keys ("get%2:29:00" "take%2:29:08" "contract%2:29:00" "take%2:39:00"  "have%2:39:06" "have%2:42:12" "have%2:30:01" "have%2:40:05" "have%2:40:03" "have%2:29:05" "have%2:35:00")
 :parent ONT::event-of-state
 :sem (F::Situation (F::Aspect F::static) (F::Time-span F::extended) (F::Trajectory -))
 :arguments ((:REQUIRED ONT::neutral ((? afh F::Phys-obj))) ;; F::Abstr-obj F::Situation)))
	     (:required ONT::neutral1 (F::Situation))
             )
 )


;; be -- this is red
(define-type ONT::HAVE-PROPERTY
 :wordnet-sense-keys ("be%2:42:03" "be%2:42:05" "savor%2:39:02" "sound%2:39:03")
 :parent ONT::event-of-state
 :sem (F::Situation (F::Aspect F::static) (F::Time-span F::extended) (F::Trajectory -))
 :arguments ((:REQUIRED ONT::neutral ((? oc1 F::Phys-obj F::Abstr-obj F::Situation)))
	     ;; this is still here until we decide what to do with the formal-pred mappings for be
	     (:optional ONT::formal ((? oc1 F::Phys-obj F::Abstr-obj F::Situation)))  
             (:ESSENTIAL ONT::PROPERTY ((? oc2 F::abstr-obj))) ;; only properties (preds) -- for event nouns use ont::have-experience or ont::participating
             )
 )

(define-type ONT::possibly-true
 :wordnet-sense-keys ("seem%2:39:01")
 :parent ONT::event-of-state
 :arguments ((:REQUIRED ONT::formal (F::situation))))


;; we have lines down; we have a book ready for you
;; 10/2009 isolating this from ont::have-property to be able to distinguish between "have" and "be" in generation
;; 12/2010 conflating this sense with ont::have
;(define-type ONT::secondary-have-property
; :parent ONT::have-property
; )

;; A root for general ownership and non-ownership: he has a dog, the computer includes a wireless card, the offer excludes existing members, the path excludes the battery
(define-type ONT::HAVING
 :parent ONT::event-of-state
 :sem (F::SITUATION (F::Aspect F::static) (F::Time-span F::Extended) (F::Trajectory -))
 :arguments ((:REQUIRED ONT::NEUTRAL ((? oc1 F::Phys-obj F::Abstr-obj F::Situation)))
	     (:ESSENTIAL ONT::NEUTRAL1 ((? oc2 F::Phys-obj F::Abstr-obj))) ;; this used to be the formal1
             )
 )

;; He has a dog, the truck has wheels -- general ownership/possession
;; the project has three programmers; the problem has a solution; your idea has a problem
;; 12/2010 we have lines down; we have a book ready for you
(define-type ONT::HAVE
 :wordnet-sense-keys ("keep%2:40:12" "keep%2:42:12" "keep%2:42:01" "take%2:42:10" "pack%2:42:00" "carry%2:42:01" "include%2:31:00" "include%2:42:00" "have%2:40:00"  "have%2:42:00"  "have%2:41:00"  "have%2:40:01" "have_got%2:40:00" "have%2:29:02" "hold%2:40:00")
    :parent ONT::HAVING
    ;; property argument needed here for the secondary predication 'we have lines down' 'we have a book ready'
    ;; added 12/2010 when conflating ont::secondary-have-property with ont::have
    ;;:arguments ((:optional ONT::PROPERTY ((? ocp F::abstr-obj))) ;; only properties (preds) -- for event nouns use ont::have-experience or ont::participating
    )

;; possess -- specific for ownership
(define-type ont::possess
 :wordnet-sense-keys ("own%2:40:00" "have%2:40:04" "possess%2:40:00" "belong%2:40:00")
  :parent ont::have
  :sem (F::SITUATION (F::Aspect F::indiv-level) (F::Time-span F::Extended))
  :arguments ((:required ont::neutral (F::phys-obj (f::intentional +))) ;; owner is typically a person or organization
	      )
  )

;; membership -- specific for membership in organizations
(define-type ont::membership
 :wordnet-sense-keys ("belong%2:42:01" "belong%2:42:07")
  :parent ont::have
  :sem (F::SITUATION (F::Aspect F::indiv-level) (F::Time-span F::Extended))
  :arguments ((:required ont::neutral (F::phys-obj (f::intentional +)))
	      (:required ont::neutral1 (F::abstr-obj (f::type ont::group-object))) ;; e.g. clubs, companies, ...
	      )
  )

(define-type ONT::wear
 :wordnet-sense-keys ("have_on%2:29:00")
 :parent ONT::event-of-action
 :arguments ((:required ont::neutral (F::phys-obj)))
 :sem (F::SITUATION (F::Aspect F::unbounded) (F::Time-span F::Extended))
 )

(define-type ONT::APPEARS-TO-HAVE-PROPERTY
 :wordnet-sense-keys ("sound%2:39:06")
 :parent ONT::HAVE-PROPERTY
 :sem (F::situation (F::Aspect F::stage-level) (F::Time-span F::extended))
 :arguments ((:OPTIONAL ONT::neutral (F::phys-obj (F::origin F::living)))
	     (:optional ont::formal (f::situation))
             )
 )

;;; predicates of comparison, e.g. equals, resembles
(define-type ONT::OBJECT-COMPARE
  :wordnet-sense-keys ("resemble%2:42:00")
 :parent ONT::event-of-state
 :sem (F::Situation (F::Trajectory -))
 :arguments ((:REQUIRED ONT::NEUTRAL ((? oc F::Phys-obj F::Abstr-obj F::Situation F::time F::proposition)))
             (:REQUIRED ONT::neutral1 ((? oc1 F::Phys-obj F::Abstr-obj F::Situation F::time F::proposition)))
             )
 )

(define-type ONT::IN-RELATION
 :wordnet-sense-keys ("diverge%2:42:00" "dominate%2:42:00" "go_by%2:32:00" "differ%2:42:00" "exceed%2:42:01" "follow%2:42:00")
 :parent ONT::OBJECT-COMPARE
 :sem (F::situation (F::Aspect F::static) (F::Time-span F::extended))
 )

(define-type ONT::comprise
 :wordnet-sense-keys ("consist%2:42:04")
 :parent ONT::in-relation
 :sem (F::situation (F::Aspect F::static) (F::Time-span F::extended))
 )

(define-type ONT::RESEMBLE
 :parent ONT::OBJECT-COMPARE
 :sem (F::Situation (F::Trajectory -))
 :arguments ((:REQUIRED ONT::FORMAL ((? oc1 F::Phys-obj F::Abstr-obj F::Situation)))
             (:REQUIRED ONT::FORMAL1)
             )
 )

(define-type ont::intentionally-act
 :parent ont::event-of-action
 :arguments ((:REQUIRED ont::action (F::Situation (F::Cause F::Agentive)))
             (:REQUIRED ONT::agent ((? ag F::phys-obj f::abstr-obj) (F::intentional +)))
             )
 )

; ont::action is used as role name for verb complements 

;; cognizer has an intention
;; aim, intend, mean
; (define-type ONT::intention
;  :wordnet-sense-keys ("specify%2:31:00" "designate%2:31:00" "destine%2:31:00" "intend%2:31:01" "intend%2:31:00" "mean%2:31:00" "think%2:31:06")
;  :parent ONT::awareness
;  )

;; cognizer has an intention
;; aim, intend, mean
(define-type ONT::intention
 :wordnet-sense-keys ("specify%2:31:00" "designate%2:31:00" "destine%2:31:00" "intend%2:31:01" "intend%2:31:00" "mean%2:31:00" "think%2:31:06")
 :parent ONT::awareness 
 ;;:parent ont::event-of-state ;; 20120529 GUM change new parent  + args
 :sem (F::Situation (F::Cause F::Mental) (F::Trajectory -))
 :arguments ((:REQUIRED ONT::Neutral ((? cg f::abstr-obj F::Phys-obj) (F::intentional +)))
             ;;; (?o (Information F_Information-content))) ;; Content/Topic
             (:ESSENTIAL ONT::Formal)
             ;;; Evidence
             (:OPTIONAL ONT::Source)
	     (:OPTIONAL ONT::Effect (f::situation)) ;; jr 20120806 adding for gloss-owl "X intend Y to Z"
             )
 )

;; e.g., my leg hurts, he itches, They tired of the game
 (define-type ONT::experiencer-obj
 :wordnet-sense-keys ("itch%2:39:02")
 :parent ONT::event-of-undergoing-action
 :sem (F::Situation (F::aspect F::dynamic) (F::cause F::agentive))
 :arguments (;; (:REQUIRED ONT::cause) ;; this used to include other verbs, but now is a formal (part or an experiencer), or experiencer
	     ;; formal can be anything -- this should rexclude the experiencer category but to do that we'd need to pull out the origin feature
             (:REQUIRED ONT::affected) ;; ie, the lungs
             )
 )

(define-type ONT::cause-body-effect
 :parent ONT::experiencer-obj
 :arguments ((:REQUIRED ONT::affected ((? cg f::abstr-obj F::Phys-obj) (F::intentional +)))
             ;;; (?o (Information F_Information-content))) ;; Content/Topic
             ;;(:ESSENTIAL ONT::cause)
	     (:ESSENTIAL ONT::agent)
	     (:OPTIONAL ont::result)
	     )
 )

;; for verbs consistent with ont::affect class but select for sentient entities in the object role -- e.g.
;; confuse, bother, annoy
(define-type ONT::affect-experiencer
 :wordnet-sense-keys ("greet%2:39:00" "come_to%2:39:00" "craze%2:37:00" "discourage%2:37:00" "deafen%2:39:01")
 :parent ONT::event-of-causation
 :sem (F::Situation (F::aspect F::dynamic) (F::cause F::agentive))
 ;; using f::origin f::living here, but this also includes plants -- no easy way to exclude those but include animals with current feature hierarchy ;-(
 :arguments ((:REQUIRED ONT::agent)
 	     (:REQUIRED ONT::affected (F::phys-obj (F::origin f::living)))
             )
 )

(define-type ont::control-manage
 :wordnet-sense-keys ("control%2:41:00" "command%2:41:00" "discharge%2:33:01" "direct%2:41:00" "drive%2:41:02")
 :parent ont::event-of-causation
 :arguments ((:REQUIRED ont::affected ((? th9 f::situation F::PHYS-OBJ F::ABSTR-OBJ)))
;	     (:REQUIRED ONT::agent ((? ag f::abstr-obj F::phys-obj) (F::intentional +)) (:implements cause))
	     (:REQUIRED ONT::agent ((? ag f::abstr-obj F::phys-obj)) (:implements cause))
             )
 )

;; release
(define-type ONT::releasing
 :wordnet-sense-keys ("free%2:41:00" "liberate%2:41:02" "release%2:41:00" "unloose%2:41:00" "unloosen%2:41:00" "loose%2:41:00" "discharge%2:41:01" "exempt%2:41:00" "let_go%2:35:00")
 :parent ONT::control-manage
 :arguments ((:OPTIONAL ONT::Instrument (F::Phys-obj (F::intentional -)))
	     (:REQUIRED ONT::affected ((? obj F::PHYS-OBJ F::ABSTR-OBJ)))
	     ;;(:OPTIONAL ONT::Cause ((? cs F::PHYS-OBJ F::ABSTR-OBJ)))
             (:REQUIRED ONT::agent ((? ag f::abstr-obj F::phys-obj) (F::intentional +)) (:implements cause))
             )
 )

;; try, attempt
(define-type ONT::TRY
 :wordnet-sense-keys ("try%1:04:00" "endeavour%1:04:00" "endeavor%1:04:00" "effort%1:04:00" "attempt%1:04:00" "try%2:29:00" "try_on%2:29:00" "taste%2:34:00" "try_out%2:34:00" "try%2:34:00" "sample%2:34:00" "essay%2:41:01" "examine%2:41:00" "try_out%2:41:00" "try%2:41:01" "prove%2:41:03" "test%2:41:00" "try%2:41:00" "seek%2:41:00" "attempt%2:41:00" "essay%2:41:00" "assay%2:41:00" "come_near%2:41:00:")
 :parent ONT::cause-effect
 :arguments ((:OPTIONAL ONT::neutral ((? agt f::abstr-obj f::situation))))
;;((:OPTIONAL ONT::Instrument (F::Phys-obj (F::intentional -)))
;	     (:REQUIRED ONT::effect (F::Situation))
;	     (:REQUIRED ONT::Formal ((? obj F::PHYS-OBJ F::ABSTR-OBJ)))
;	     (:OPTIONAL ONT::Cause)
;	     (:REQUIRED ONT::agent ((? ag f::abstr-obj F::phys-obj) (F::intentional +)) (:implements cause))
;             )
 )

(define-type ONT::TRY-AGAIN
 :parent ONT::try
 )


;; fail, neglect
;; this isn't a subtype of ont::try because the intentionality is indeterminate
(define-type ONT::fail
 :wordnet-sense-keys ("bomb%2:41:00" "fail%2:30:07" "fail%2:40:00" "fail%2:41:00" "fail%2:41:02" "fail%2:41:08" "fail%2:41:12" "fall_short_of%2:42:00" "neglect%2:31:05")
 :parent ONT::acting
 :arguments ((:OPTIONAL ONT::neutral)
	     (:REQUIRED ONT::formal (F::Situation))
	     (:REQUIRED ONT::affected ((? obj F::PHYS-OBJ F::ABSTR-OBJ)))
	     (:OPTIONAL ONT::agent)
	     )
 )

; (define-type ont::miss
;     :wordnet-sense-keys ("miss%2:35:00" "miss%2:42:02")
;     :parent ONT::acting
;     :arguments ((:OPTIONAL ONT::Instrument (F::Phys-obj (F::intentional -)))
; 		(:REQUIRED ONT::affected ((? obj F::PHYS-OBJ F::ABSTR-OBJ)))
; 		(:OPTIONAL ONT::agent)
; 		)
;     )

(define-type ont::miss
    :parent ONT::acting
    :arguments ((:OPTIONAL ONT::Instrument (F::Phys-obj (F::intentional -)))
		(:REQUIRED ONT::affected)
		)
 )

(define-type ONT::succeed
 :wordnet-sense-keys ("hold_one's_own%2:42:00" "succeed%2:41:00")
 :parent ONT::acting
 :arguments ((:OPTIONAL ONT::Instrument (F::Phys-obj (F::intentional -)))
	     (:REQUIRED ONT::effect (F::Situation))
	     (:OPTIONAL ONT::agent)
	     ;;(:REQUIRED ONT::agent ((? ag f::abstr-obj F::phys-obj) (F::intentional +)) (:implements cause))
              )
 )


;; tend
(define-type ONT::be-inclined
    ;; 20120529 GUM change new parent
    :wordnet-sense-keys ("tend%2:42:01")
    :parent ont::event-of-state
    :arguments (
		(:REQUIRED ONT::effect (F::Situation))
		(:REQUIRED ONT::Formal ((? obj F::PHYS-OBJ F::ABSTR-OBJ) (f::intentional -)))
		)
    )

(define-type ONT::Choosing
 :parent ONT::intentionally-act
 :sem (F::Situation (F::Cause F::Agentive))
 :arguments ((:ESSENTIAL ONT::Agent ((? obj F::PHYS-OBJ F::ABSTR-OBJ) (F::Intentional +)))
             (:REQUIRED ONT::affected) ;; unrestricted -- you can choose anything, including times
             (:OPTIONAl ont::formal)
	     (:optional ont::neutral)
             )
 )

(define-type ONT::ACQUIRE
 :wordnet-sense-keys ("take%2:33:08" "have%2:35:00" "take%2:35:14" "take%2:40:08" "take%2:40:04" "subscribe_to%2:40:00" "subscribe%2:40:00" "exact%2:32:01" "take%2:32:00" "claim%2:32:01" "have%2:40:05" "take%2:40:05" "accept%2:40:00" "take%2:40:00" "take%2:30:01" "take_on%2:30:00" "adopt%2:30:01" "acquire%2:30:00" "assume%2:30:00" "get%2:40:00" "acquire%2:40:00" "take_up%2:35:01" "take_in%2:35:02" "advance%2:33:00" "bring%2:40:00" "bring_home_the_bacon%2:41:00" "win%2:33:00")
 :parent ONT::EVENT-OF-CAUSAtion
 :sem (F::SITUATION (F::Cause F::Agentive) (F::Trajectory -) (F::Aspect F::Dynamic))
 :arguments ((:REQUIRED ONT::agent ((? ag f::abstr-obj F::phys-obj) (F::intentional +)))
             (:REQUIRED ONT::affected ((? th10  F::Phys-obj F::Abstr-obj F::situation)))
	     (:optional ont::source)
	     (:optional ont::result)
	     (:optional ont::affected-result)
	     )
 )


