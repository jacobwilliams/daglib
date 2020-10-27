Overview
========

DAG is a Fortran 2018 library for creating and manipulating directed acyclic graphs (DAGs).
It includes a topological sort feature, and it generates files in the [GraphViz] "dot" format.

Prerequisites
-------------
The recommended versions below are the versions used in developing dag.  Earlier versions
might work also.

1. A Fortran 2018 compiler (Recommended: [gfortran] 10 or later).
2. [OpenCoarrays]  (Recommended: 2.8.0 or later)
3. [CMake]  (Recommended: 3.17 or later)

Building and testing
--------------------
### Serial builds
Replace caf with gfortran in the parallel build instructions below.

### Parallel builds
To clone, build, and test, execute the following in a `bash` shell:
```
git clone https://github.com/sourceryinstitute/dag
mkdir -p dag/build
cd dag/build
export FC=caf
cmake .. -DCMAKE_PREFIX_PATH=<insert-gFTL-gFTL-shared-yaFyaml-installation-root-directory-here>
ctest
```
or the corresponding commands for other shells.

Users who prefer a [FoBiS] build system, please see [daglib by Jacob Williams], from which
the current repository was forked.

Example
-------

The [jacob-example] test provides a short example of the use of dag, including checks

for the expected results.  That test also writes the following image to a `.png` file:

<img src="https://raw.githubusercontent.com/sourceryinstitute/dag/master/media/dag_example.png" width="500">

License
-------

This library is released under a [BSD-3 license].

[daglib by Jacob Williams]: https://github.com/jacobwilliams/daglib
[FoBiS]: https://github.com/szaghi/FoBiS
[GraphViz]: https://www.graphviz.org
[jacob-example]: https://github.com/sourceryinstitute/dag/blob/master/tests/integration/jacob-example/test-jacob-example.f90
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[CMake]: https://www.cmake.org
[gfortran]: https://gcc.gnu.org
[BSD-3 license]: https://github.com/sourceryinstitute/dag/blob/master/LICENSE
