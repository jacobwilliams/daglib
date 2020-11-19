module dag_input_output_test
  !!  Test DAG input/output.
  implicit none

contains

  function check_input_output() result(io_result)
    use dag_interface, only : dag
    use vegetables, only : result_t, succeed

    type(result_t) io_result
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

      if (this_image()==1) then

        open(newunit=file_unit, file="output/dag-output.json", status="unknown", iostat=io_status, iomsg=error_message)
        io_result = asertEquals(io_status,0)

        write(file_unit,*) dependency_graph
        close(file_unit)
      end if
    end block

  end function

end module dag_input_output_test
