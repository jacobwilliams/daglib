add_library(dag
  dag_interface.f90
  dag_implementation.F90
  vertex_interface.F90
  vertex_implementation.f90
  assert_implementation.F90
  assert_interface.F90
)

set(install_dir "DAG-${DAG_VERSION}")
set(dest "${CMAKE_INSTALL_PREFIX}/${install_dir}")

target_include_directories (dag PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for .mod files when building
  $<INSTALL_INTERFACE:${install_dir}/mod> # for client in install mode
)

target_link_libraries(dag
  PRIVATE yafyaml
)
set_target_properties(dag
 PROPERTIES Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/mod
)


install(TARGETS dag
  DESTINATION "${dest}/lib"
  EXPORT DAGConfig
)
install(DIRECTORY ${PROJECT_BINARY_DIR}/mod
  DESTINATION "${dest}"
)
export(TARGETS dag
  FILE "DAGConfig.cmake"
)
install(EXPORT DAGConfig
  DESTINATION "${dest}/cmake"
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
