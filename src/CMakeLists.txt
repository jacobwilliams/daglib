add_library(dag
  dag_interface.f90
  dag_implementation.F90
  vertex_interface.F90
  vertex_implementation.f90
  assert_implementation.F90
  assert_interface.F90
)
target_link_libraries(dag
  PRIVATE yafyaml
)
target_include_directories(dag
  PUBLIC $<TARGET_PROPERTY:yafyaml,Fortran_MODULE_DIRECTORY>
)

install(TARGETS dag
  EXPORT dagConfig
)
export(TARGETS dag
  FILE "${CMAKE_CURRENT_BINARY_DIR}/dagConfig.cmake"
)
install(EXPORT dagConfig
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
