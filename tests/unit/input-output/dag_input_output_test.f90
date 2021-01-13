module dag_input_output_test
  !!  Test DAG input/output.
  implicit none
  private

  public :: test_dag_input_output
contains
  function test_dag_input_output() result(tests)
    use vegetables_m, only: TestItem_t, describe, it

    type(TestItem_t) :: tests

    tests = describe("a dependency graph", &
        [it("can be written to a file", check_input_output)])
  end function

  function check_input_output() result(io_result)
    use dag_interface, only : dag
    use vegetables_m, only : result_t, assertEquals, succeed

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

      if (this_image()/=1) then
        io_result = succeed("no writing to do on images other than image 1")
      else

        open(newunit=file_unit, file="output/dag-output.json", status="unknown", iostat=io_status, iomsg=error_message)
        io_result = assertEquals(io_status,0, "opening the file was successful", "openning the file failed")

        write(file_unit,*) dependency_graph
        close(file_unit)
      end if

    end block

  end function

end module dag_input_output_test
