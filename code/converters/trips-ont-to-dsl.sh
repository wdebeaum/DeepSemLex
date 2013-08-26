#!/bin/bash

export TRIPS_BASE=../../../..
export CONFIGDIR=$TRIPS_BASE/src/config
export LISP=`grep -P -o -e '(?<=^LISP = ).*' $CONFIGDIR/lisp/defs.mk`
$LISP \
  --noinform \
  --noprint \
  --disable-debugger \
  --load trips-ont-to-dsl.lisp \
  --eval '(run)' \
| grep -v '^;'

