program dag_input
  !!  Test DAG input/output.
  use dag_interface, only : dag
  use assert_interface, only : assert
  implicit none

  type(dag) :: dependency_graph

  call dependency_graph%input("dag-vertices-edges.json")

  if ( any(dependency_graph%get_edges(1) /= [2,3]      )) error stop "dag%get_edges(1) incorrect"
  if ( any(dependency_graph%get_edges(2) /= [3]        )) error stop "dag%get_edges(2) incorrect"
  if ( any(dependency_graph%get_edges(3) /= [integer::])) error stop "dag%get_edges(3) incorrect"

  block
    integer file_unit, io_status
    integer, parameter :: success = 0
    character(len=128) :: error_message

    open(newunit=file_unit, file="dag-output.json", status="unknown", iostat=io_status, iomsg=error_message)
    call assert(io_status==success, "io_status==0", error_message)

    write(file_unit,*) dependency_graph
    close(file_unit)
  end block

  sync all
  if (this_image()==1) print *,"Test passed: input-output"

end program dag_input
