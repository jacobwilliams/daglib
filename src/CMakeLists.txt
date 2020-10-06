add_library(dag
  dag_interface.f90
  dag_implementation.F90
  vertex_interface.F90
  vertex_implementation.f90
  assert_implementation.F90
  assert_interface.F90
)
set_target_properties(dag
  PROPERTIES Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include
)
target_link_libraries(dag
  PRIVATE yafyaml
)
set (dest
  "DAG-${DAG_VERSION_MAJOR}.${DAG_VERSION_MINOR}"
)
install(TARGETS dag
  DESTINATION ${dest}/lib
)
install(DIRECTORY ${PROJECT_BINARY_DIR}/include/
  DESTINATION ${dest}/include
)

#Toggle C preprocessor macro for turning assertions on or off
if(NO_ASSERTIONS)
  target_compile_definitions( dag
    PUBLIC "-DUSE_ASSERTIONS=.false."
  )
else()
  target_compile_definitions( dag
    PUBLIC "-DUSE_ASSERTIONS=.true."
  )
endif()

include(CheckFortranSourceCompiles)
