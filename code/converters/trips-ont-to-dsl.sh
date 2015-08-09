#!/bin/bash

export TRIPS_BASE=../../trips
source ../lisp-env.sh
$LISP \
  $BATCH \
  $LOAD trips-ont-to-dsl.lisp \
  $EVAL '(run)' \
| grep -v '^;'

