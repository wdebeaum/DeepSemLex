#!/bin/bash

output_dir=$1

CONFIGDIR="$TRIPS_BASE/src/config"

# get environment from lisp/defs.mk
`awk 'BEGIN { print "export" } /^([A-Z_]+) = ([^[:space:]]+)$/ { print " " $1 "=" $3 }' <$CONFIGDIR/lisp/defs.mk`

# someday I'd like these to be in lisp/defs.mk, but that day hasn't come yet
BATCH=--batch
LOAD=--load
EVAL=--eval
QUIT="--eval (quit)"

case "$LISP_FLAVOR" in
  sbcl)
    BATCH="--noinform --noprint --disable-debugger"
    QUIT="--eval (sb-ext:quit)"
    ;;
  ccl)
    LOAD=-l
    EVAL=-e
    QUIT="-e (quit)"
    ;;
  cmucl)
    BATCH=-batch
    LOAD=-load
    EVAL=-eval
    QUIT="-eval (quit)"
    ;;
  allegro)
    BATCH=-batch -q
    LOAD=-L
    EVAL=-e
    QUIT="-e (exit)"
    ;;
  *)
    echo "Warning: unknown lisp flavor $LISP_FLAVOR" >&2
esac

# run genericized lisp command
$LISP \
  $BATCH \
  $LOAD write-trips-ont-xmls.lisp \
  $EVAL "(run \"$output_dir\")" \
  $QUIT

