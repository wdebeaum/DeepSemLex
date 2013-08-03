#!/bin/bash

export TRIPS_BASE=../../../..
export CONFIGDIR=$TRIPS_BASE/src/config
export LISP=`grep -P -o -e '(?<=^LISP = ).*' $CONFIGDIR/lisp/defs.mk`
export RUBY=`grep -P -o -e '(?<=^RUBY  = ).*' $CONFIGDIR/ruby/defs.mk`

grep -P -o -e '\(DEFINE-CONCEPT .*\)(?=\)$)' \
| uniq \
| $LISP \
  --noinform \
  --noprint \
  --disable-debugger \
  --load gloss-output-to-dsl.lisp \
  --eval '(run)' \
| $RUBY gloss-output-to-dsl.rb

# TODO
# ? include lattice that gloss used as input (have to get it from somewhere other than facilitator.log)
# ? get rid of excessive ONT:: prefixes in LF terms
