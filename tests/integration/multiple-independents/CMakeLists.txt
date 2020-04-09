add_executable( test-multiple-independents
 test-multiple-independents.f90
)
target_link_libraries( test-multiple-independents
  PRIVATE dag
)
