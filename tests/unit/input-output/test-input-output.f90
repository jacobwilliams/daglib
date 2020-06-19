program dag_input
  !!  Test DAG input/output.
  use dag_interface, only : dag
  implicit none

  type(dag) :: dependency_graph

  call dependency_graph%input("dag-vertices-edges.json")

  sync all
  if (this_image()==1) print *,"Test passed."

end program dag_input
