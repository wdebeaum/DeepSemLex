;;;; important types of symbols
(in-package :dsl)

;;; lists from querying old lexicon (probably not actually complete)

(deftype pos ()
  '(member adj adj art conj FP infinitival-to N name neg number-unit ordinal prep pro punc quan uttword V value))

(deftype syn-cat ()
  '(or pos (member adjp advbl CP NP number PP pred S utt VP VP- word)))

(deftype syn-arg ()
  '(member lsubj lobj liobj comp subcat subcat2 post-subcat premod argument))

(deftype syn-feat ()
  '(member
  ; key		; values (unless just + -)
    agr		; 1p 1s 2p 2s 3p 3s
    allow-deleted-comp
    allow-post-n1-subcat
    atype 	; post pre pre-vp
    case	; - obj sub
    comparative	; + superl
    comp-op	; - less
    conj
    contraction
    diectic
    disj
    else-word
    functn	; acceptability-val compare-val linear-scale
    lex		; us you
    mass	; bare mass count
    neg
    negatable
    npmod
    pointer
    pos		; (see deftype pos)
    quant
    refl
    seq
    sing-lf-only
    status	; definite-plural indefinite indefinite-plural quantifier
    stem	; he her him it me one she they we who you
    vform	; base fut pres ing past pastpart passive
    wh 		; q r (question, relative clause)
    ))

;;; sem-feat subtypes

(deftype phys-obj-feat ()
  '(member object-function origin form mobility group spatial-abstraction intentional information container kr-type trajectory))

(deftype situation-feat ()
  '(member aspect time-span cause trajectory locative intentional information container kr-type type origin))

(deftype abstr-obj-feat ()
  '(member measure-function scale intentional information container gradability kr-type object-function origin intensity orientation))

(deftype proposition-feat ()
  '(member intentional information container gradability kr-type origin))

(deftype time-feat ()
  '(member time-function scale time-scale kr-type))

(deftype sem-feat ()
  '(or phys-obj-feat situation-feat abstr-obj-feat proposition-feat time-feat))

;;; feature list types

(deftype feats ()
  '(alist :to (maybe-disj symbol)))

;; FIXME according to the UML diagram, syn-feats and sem-feats are full-fledged
;; concept classes, not just alists. Either the UML diagram or the Lisp code
;; should change.
(deftype syn-feats ()
  '(alist :from syn-feat :to (maybe-disj symbol)))

(deftype sem-feats ()
  '(alist :from sem-feat :to (maybe-disj symbol)))

;;; list from VerbNet

(deftype sem-role ()
  '(member Agent Asset Attribute Beneficiary Cause Co-Agent Co-Patient Co-Theme Destination Experiencer Extent Goal Initial_Location Instrument Location Material Patient Pivot Predicate Product Recipient Result Source Stimulus Theme Time Topic Trajectory Value))

