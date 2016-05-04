# DeepSemLex #

This README is for the standalone version of the Deep Semantic Lexicon lisp library. Normally this library operates in TRIPS (The Rochester Interactive Planning System) as a "component", meaning it communicates in KQML (Knowledge Query and Manipulation Language) over a socket with other components/modules. It has some dependencies that are part of TRIPS, and are included here in the `trips/` directory.

## Build instructions ##

    ./configure
    make

`make install` might put something in `trips/etc/` but it's not necessary at this stage of development.

To run:

    cd code/lib/
    lisp
    (load "defsys") ; load lisp system definition
    (dfc:load-component :deepsemlex) ; load the system itself
    (in-package :dsl) ; switch to the right package for the following
    (require-resource-version :ont) ; load TRIPS ontology (ONT types)
    (require-resource-version :ont-t) ; load TRIPS templates
    (require-resource-version :ont-w) ; load TRIPS lexicon (words)

## More info ##

For a gist of how DSL's database is organized, see `docs/dsl-uml-class-diagram.pdf`.

See also [docs/README.html](docs/README.html), which is written in the context of TRIPS. For reference, `$TRIPS_BASE` is `trips/`, and in TRIPS the rest of this repository would live in `$TRIPS_BASE/src/DeepSemLex/`.

Note that the `trips/src/config/lisp/defsystem/defsystem-3.6i/` directory contains a modified, non-standard, non-official version of [MK:DEFSYSTEM](http://www.cliki.net/mk-defsystem) 3.6i. See the comments near the top of `defsystem.lisp` in that directory for its copyright notice and license.

The rest of the repository is licensed using the [GPL 2+](http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) (see `gpl-2.0.txt`):

DeepSemLex - TRIPS deep semantic lexicon lisp library  
Copyright (C) 2016  Institute for Human & Machine Cognition

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
