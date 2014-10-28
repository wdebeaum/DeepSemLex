;;;; important types of symbols
(in-package :dsl)

;;; lists from querying old lexicon (probably not actually complete)

(deftype pos ()
  '(member adj adv art conj FP infinitival-to N name neg number-unit ordinal prep pro punc quan uttword V value))

(deftype syn-cat ()
  '(or pos (member adjp advbl CP NP number PP pred S utt VP VP- word)))

(deftype syn-arg ()
  '(member lsubj lobj liobj lcomp subcat subcat2 post-subcat premod argument))

(deftype syn-feat ()
  '(member
  ; key		; values (unless just + -)
    agr		; 1p 1s 2p 2s 3p 3s
    allow-before
    allow-deleted-comp
    allow-post-n1-subcat
    atype 	; post pre pre-vp
    aux
    auxname	; - passive progr perf
    cardinality
    case	; - obj sub
    changesem
    comparative	; + superl
    comp-op	; - less
    conj
    contraction
    diectic
    disj
    ellipsis
    else-word
    ;; Form wasn't originally an explicit feature, just passed around inside
    ;; LXM, but I think it makes sense as a feature. Note that this only covers
    ;; inflections, not cross-POS derivations like :nom, :result, and :ly
    form	; none 12s123pbase 3s ing past pastpart sing plur er est
    functn	; acceptability-val compare-val linear-scale
    gap
    generated
    implicit-arg
    indef-only
    lex		; us you
    mass	; bare mass count
    modal
    name
    nname
    neg
    negatable
    npmod
    nobarespec
    npagr	; ???
    number
    numerical-form	; numercal-both numerical-comp
    operator	; one-of
    pointer
    pos		; (see deftype pos)
    poss
    pro		; + indef recip
    ptype	; (prepositions)
;    qcomp	; (constits (blech))
;    qof		; (ditto)
    qtype	; wh
    quant
    refl
    seq
    sing-lf-only
    skip
    sort	; attribute-unit binary-constraint classifier disc double-subcat else number-unit operator other-reln pp-word pred substance-unit unit-measure
    status	; definite-plural indefinite indefinite-plural quantifier
    stem	; he her him it me one she they we who you
    subcat	; - s pp np vp adjp
    subcat1	;
    subcat2	;
    subcat3	;
    unaccusative
    vform	; base fut pres ing past pastpart passive
    wh 		; q r (question, relative clause)
    ^s-plural
    ))

;;; sem-feat subtypes

(deftype phys-obj-feat ()
  '(member object-function origin form mobility group spatial-abstraction intentional information container kr-type trajectory))

(deftype situation-feat ()
  '(member aspect time-span cause trajectory locative intentional information container kr-type type origin iobj))

(deftype abstr-obj-feat ()
  '(member measure-function scale intentional information container gradability kr-type object-function origin intensity orientation))

(deftype proposition-feat ()
  '(member intentional information container gradability kr-type origin))

(deftype time-feat ()
  '(member time-function scale time-scale kr-type))

(in-package :vn)

  (common-lisp::deftype sem-feat ()
    ;; this list generated by getting all the unique type attributes of SELRESTR
    ;; elements in the VerbNet data
    '(common-lisp::member abstract animal animate body_part comestible communication concrete currency dest dest_conf dest_dir dir elongated force garment human int_control loc location machine nonrigid organization path plural pointy refl region scalar solid sound spatial src state substance time vehicle))

  (common-lisp::deftype sem-role ()
    '(common-lisp::member Agent Asset Attribute Beneficiary Cause Co-Agent Co-Patient Co-Theme Destination Experiencer Extent Goal Initial_Location Instrument Location Material Patient Pivot Predicate Product Recipient Result Source Stimulus Theme Time Topic Trajectory Value))

(common-lisp::in-package :dsl)

(deftype sem-feat ()
  '(or (member type) phys-obj-feat situation-feat abstr-obj-feat
       proposition-feat time-feat vn::sem-feat))

;;; feature list types

(deftype feats (&optional (feat-type 'symbol))
  `(alist :from ,feat-type :to (maybe-disj symbol)))

(in-package :ont)

  (common-lisp::deftype sem-role ()
    ;; this list found by grepping templates and OM files (see get-ont-sem-roles.sh)
    '(common-lisp::member action addressee affected affected1 agent agent1 along arg0 arg1 assoc-with associated-information beneficiary cause co-agent co-result co-theme cognizer content contents cost criterion donor duration effect effect-implicit entity experiencer extent figure formal from-loc goal goal-reln goods ground instrument interval location manner material money neutral neutral1 neutral2 norole obj-val of of-state of1 of2 originator path place position-reln predicate property purpose purpose-implicit result result-val sit-val situation source source-reln spatial-loc stative stimulus theme time-duration-rel time-val to-loc val val2 value via 
    ;; some extras output by src/Systems/gloss/test.lisp
    co-theme patient partner
    ))

(common-lisp::in-package :dsl)

(deftype sem-role ()
  '(or vn::sem-role ont::sem-role))

