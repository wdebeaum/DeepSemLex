#!/bin/bash

export TRIPS_BASE=../../trips
source ../lisp-env.sh
$LISP \
  $BATCH \
  $LOAD trips-ont-to-dsl-inh.lisp \
  $EVAL '(run)' \
| grep -v '^;'

