module dag_input_output_test
  use dag_interface, only: dag_t
  use vegetables, only: &
      result_t, test_item_t, assert_equals, describe, it, succeed
  !!  Test DAG input/output.
  implicit none
  private

  public :: test_dag_input_output
contains
  function test_dag_input_output() result(tests)
    type(test_item_t) :: tests

    tests = describe("a dependency graph", &
        [it("can be written to a file", check_input_output)])
  end function

  function check_input_output() result(io_result)
    type(result_t) io_result
    type(dag_t) :: dependency_graph
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
        io_result = assert_equals(0, io_status, "opening the file was successful", "openning the file failed")

        write(file_unit,*) dependency_graph
        close(file_unit)
      end if

    end block

  end function

end module dag_input_output_test
