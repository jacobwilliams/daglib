add_executable( dag_example
 dag_example.f90
)
target_link_libraries( dag_example
  PRIVATE dag
)
