add_executable( test-input-output
 test-input-output.f90
)
target_link_libraries( test-input-output
  PRIVATE dag
)
file(COPY dag-vertices-edges.json DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

