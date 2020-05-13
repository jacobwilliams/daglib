add_executable( test-input-output
 test-input-output.f90
)
target_link_libraries( test-input-output
  PRIVATE dag
  PRIVATE jsonfortran-static
)
target_include_directories(jsonfortran-static
  PUBLIC $<TARGET_PROPERTY:jsonfortran-static,Fortran_MODULE_DIRECTORY>
)
