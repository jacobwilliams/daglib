add_library(dag
  dag_interface.f90
  dag_implementation.F90
  vertex_interface.F90
  vertex_implementation.f90
  assert_implementation.f90
  assert_interface.f90
)
#target_link_libraries(dag
#  PRIVATE yafyaml
#)
#target_include_directories(dag
#  PUBLIC $<TARGET_PROPERTY:yafyaml,Fortran_MODULE_DIRECTORY>
#)
