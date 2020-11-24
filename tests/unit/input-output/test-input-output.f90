program dag_input_output
  !!  Test DAG input/output.
  use dag_interface, only : dag
  use assert_interface, only : assert
  implicit none

  type(dag) :: dependency_graph
  integer, parameter :: n_nodes = 6, success=0
  integer istat

  call dependency_graph%set_vertices(n_nodes)
  call dependency_graph%set_edges(2,[1])     !2 depends on 1
  call dependency_graph%set_edges(3,[5,1])   !3 depends on 5 and 1
  call dependency_graph%set_edges(4,[5])     !4 depends on 5
  call dependency_graph%set_edges(5,[2])     !5 depends on 2
  call dependency_graph%set_edges(6,[2,4])   !6 depends on 2 and 4

  block
    integer file_unit, io_status
    integer, parameter :: success = 0
    character(len=128) :: error_message

    open(newunit=file_unit, file="output/dag-output.json", status="unknown", iostat=io_status, iomsg=error_message)
    call assert(io_status==success, "io_status==0", error_message)

    write(file_unit,*) dependency_graph
    close(file_unit)
  end block

  sync all
  if (this_image()==1) print *,"Test passed: input-output"

end program dag_input_output
