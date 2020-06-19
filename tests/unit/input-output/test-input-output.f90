program dag_input
  !!  Test DAG input/output.
  use dag_interface, only : dag
  implicit none

  type(dag) :: dependency_graph

  call dependency_graph%input("dag-vertices-edges.json")

  if ( any(dependency_graph%get_edges(1) /= [2,3]      )) error stop "dag%get_edges(1) incorrect"
  if ( any(dependency_graph%get_edges(2) /= [3]        )) error stop "dag%get_edges(2) incorrect"
  if ( any(dependency_graph%get_edges(3) /= [integer::])) error stop "dag%get_edges(3) incorrect"

  sync all
  if (this_image()==1) print *,"Test passed."

end program dag_input
