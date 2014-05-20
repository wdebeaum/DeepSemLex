#!/bin/bash

output_dir=$1

export TRIPS_BASE=../../../..
export CONFIGDIR=$TRIPS_BASE/src/config
export LISP=`grep -P -o -e '(?<=^LISP = ).*' $CONFIGDIR/lisp/defs.mk`
$LISP \
  --noinform \
  --noprint \
  --disable-debugger \
  --load write-trips-ont-xmls.lisp \
  --eval "(run \"$output_dir\")" \
  --eval '(sb-ext:quit)'

