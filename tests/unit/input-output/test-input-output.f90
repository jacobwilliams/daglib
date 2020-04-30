program dag_input_output
  !!  Test DAG input/output.
  use iso_fortran_env, only : error_unit
  use dag_interface, only : dag
  implicit none

  type(dag) :: dependency_graph

  integer,parameter :: n_nodes = 14, success=0

  call dependency_graph%set_vertices(n_nodes)
  call dependency_graph%set_edges( 2,[integer::]) !2 depends on nothing
  call dependency_graph%set_edges( 3,[integer::]) !3 depends on nothing
  call dependency_graph%set_edges( 4,[3])   !4 depends on  3
  call dependency_graph%set_edges( 5,[4])   !5 depends on  4
  call dependency_graph%set_edges( 6,[3])   !6 depends on  3
  call dependency_graph%set_edges( 7,[4])   !2 depends on  4
  call dependency_graph%set_edges( 8,[5])   !3 depends on  5
  call dependency_graph%set_edges( 9,[6])   !4 depends on  6
  call dependency_graph%set_edges(10,[6,7]) !5 depends on  6 and 7
  call dependency_graph%set_edges(11,[7,8]) !6 depends on  7 and 8
  call dependency_graph%set_edges(12,[9])   !2 depends on  9
  call dependency_graph%set_edges(13,[10])  !3 depends on 10
  call dependency_graph%set_edges(14,[11])  !4 depends on 11

  sync all
  if (this_image()==1) print *,"Test passed."

end program dag_input_output
