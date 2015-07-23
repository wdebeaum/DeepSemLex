# DeepSemLex #

## Build instructions ##

    ./configure
    make

`make install` might put something in `trips/etc/` but it's not necessary at this stage of development.

To run:

    cd code/lib/
    lisp
    (load "defsys")
    (in-package :dsl)

