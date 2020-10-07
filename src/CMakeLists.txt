add_library(dag
  dag_interface.f90
  dag_implementation.F90
  vertex_interface.F90
  vertex_implementation.f90
  assert_implementation.F90
  assert_interface.F90
)
#target_include_directories (dag
  #$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for headers when building
  #$<INSTALL_INTERFACE:"DAG-${DAG_VERSION_MAJOR}.${DAG_VERSION_MINOR}/include"> # for client in install mode
#)

#set_target_properties(dag
# PROPERTIES Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include
#
target_link_libraries(dag
  PRIVATE yafyaml
)
install(TARGETS dag
  DESTINATION ${PROJECT_BINARY_DIR}/lib
  EXPORT DAGConfig
)
install(DIRECTORY ${PROJECT_BINARY_DIR}/mod/
  DESTINATION mod
)
export(TARGETS dag
  FILE "${CMAKE_CURRENT_BINARY_DIR}/DAGConfig.cmake"
)
install(EXPORT DAGConfig
  DESTINATION "${CMAKE_INSTALL_PREFIX}/dag/cmake"
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
