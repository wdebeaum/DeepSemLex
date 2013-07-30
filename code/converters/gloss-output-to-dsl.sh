#!/bin/bash

export LISP=`grep -P -o -e '(?<=^LISP = ).*' ../../../config/lisp/defs.mk`

grep -P -o -e '\(DEFINE-CONCEPT .*\)(?=\)$)' \
| uniq \
| $LISP \
  --noinform \
  --noprint \
  --disable-debugger \
  --load gloss-output-to-dsl.lisp \
  --eval '(run)'

# TODO
# - correct usage of sense keys to identify synsets
#  - top-level concept name should be synset
#  - restrictions should be synsets
#  - :WNSENSE should be the sense key for the actual word used in the term
# ? include lattice that gloss used as input (have to get it from somewhere other than facilitator.log)
# ? get rid of excessive ONT:: prefixes in LF terms
