add_executable( depdat_test
 depdat_test.f90
)
target_link_libraries( depdat_test
  PRIVATE dag
)
