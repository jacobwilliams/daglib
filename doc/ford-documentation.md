project: Directed Acyclic Graph Library
summary: A Fortran 2018 application programmer interface for representing directed acyclic graphs.
src_dir: ../src
src_dir: ../tests
output_dir: html
preprocess: true
macro: FORD
preprocessor: gfortran -E
display: public
         protected
         private
source: true
graph: true
md_extensions: markdown.extensions.toc
coloured_edges: true
sort: permission-alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html#ISO_005fC_005fBINDING
project_github: https://github.com/sourceryinstitute/dag
author: Jacob Williams and Damian Rouson
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
project_github: https://github.com/sourceryinstitute/dag
project_download: https://github.com/sourceryinstitute/releases
github: https://github.com/sourceryinstitute
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !

{!../README.md!}
