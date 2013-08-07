#!/bin/bash

export TRIPS_BASE=../../../..
export CONFIGDIR=$TRIPS_BASE/src/config
export LISP=`grep -P -o -e '(?<=^LISP = ).*' $CONFIGDIR/lisp/defs.mk`
export RUBY=`grep -P -o -e '(?<=^RUBY  = ).*' $CONFIGDIR/ruby/defs.mk`
export PERL=`grep -P -o -e '(?<=^PERL  = ).*' $CONFIGDIR/perl/defs.mk`

# Note: can't use uniq to remove duplicate lines, because their length may
# exceed LINE_MAX, and uniq on Mac OS X truncates such lines.
grep -P -o -e '\(DEFINE-CONCEPT .*\)(?=\)$)' \
| $PERL -n -e 'print unless ($_ eq $prev); $prev = $_;' \
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
