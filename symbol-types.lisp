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
  '(member pos lex status stem agr wh vform mass atype case functn comparative comp-op))

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

(deftype syn-feats ()
  '(alist :from syn-feat :to (maybe-disj symbol)))

(deftype sem-feats ()
  '(alist :from sem-feat :to (maybe-disj symbol)))

;;; list from VerbNet

(deftype sem-role ()
  '(member actor actor1 actor2 agent asset attribute beneficiary destination experiencer instrument location material patient patient1 patient2 product recipient source theme theme1 theme2 time topic value))

