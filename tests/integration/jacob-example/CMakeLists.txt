add_executable( test-jacob-example
  test-jacob-example.f90
)
target_link_libraries( test-jacob-example
  PRIVATE dag
)
