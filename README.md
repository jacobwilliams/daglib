DAGLILB
=============================

## Overview

A simple module for creating and manipulating directed acyclic graphics (DAGs).

## Building

A [FoBiS](https://github.com/szaghi/FoBiS) configuration file (`daglib.fobis`) is provided that can build the library and examples. Use the `mode` flag to indicate what to build. For example:

* To build all the examples using gfortran: `FoBiS.py build -f daglib.fobis -mode tests-gnu`
* To build all the examples using ifort: `FoBiS.py build -f daglib.fobis -mode tests-intel`
* To build a static library using gfortran: `FoBiS.py build -f daglib.fobis -mode static-gnu`
* To build a static library using ifort: `FoBiS.py build -f daglib.fobis -mode static-intel`

The full set of modes are:

* `static-gnu`
* `static-gnu-debug`
* `static-intel`
* `static-intel-debug`
* `shared-gnu`
* `shared-gnu-debug`
* `shared-intel`
* `shared-intel-debug`
* `tests-gnu`
* `tests-gnu-debug`
* `tests-intel`
* `tests-intel-debug`

### License

This library is released under a [BSD-3 license](https://github.com/jacobwilliams/daglib/blob/master/LICENSE).

