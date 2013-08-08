#!/bin/bash

export TRIPS_BASE=../../../..
export CONFIGDIR=$TRIPS_BASE/src/config
export LISP=`grep -P -o -e '(?<=^LISP = ).*' $CONFIGDIR/lisp/defs.mk`
$LISP \
  --noinform \
  --noprint \
  --disable-debugger \
  --load dsl-to-xml.lisp \
  --eval '(run)' \
| grep -v '^;'

