module dag_test
  use dag_interface, only: dag_t
  use vertex_interface, only: vertex_t
  use vegetables, only: &
      result_t, test_item_t, assert_equals, describe, it, succeed
  use iso_varying_string, only : varying_string, var_str, assignment(=)
  !!  Test DAG construction, input, and output.
  implicit none
  private
  public :: test_dag_construction

contains

  function test_dag_construction() result(tests)
    type(test_item_t) :: tests

    tests = describe("dag's module dependency graph", &
        [it("can be constructed and output to .dot file that can be rendered as a PDF", check_dag_construction)])
  end function

  function check_dag_construction() result(result_)
    type(result_t) result_

    enum, bind(C)
      enumerator :: assert_m=1, vertex_m, vertex_s, dag_m, dag_s 
    end enum
    
    type(varying_string) :: names(size([assert_m, vertex_m, vertex_s, dag_m, dag_s ]))
 
    names(assert_m) = "assert_m"
    names(vertex_m) = "vertex_m"
    names(vertex_s) = "vertex_s" 
    names(dag_m)    = "dag_m"
    names(dag_s)    = "dag_s"

    block 
      character(len=*), parameter :: &
         branch    = 'shape=square, fillcolor="SlateGray1", style=filled' &
        ,external_ = 'shape=square, fillcolor="green",      style=filled' &
        ,root      = 'shape=circle, fillcolor="white",      style=filled' &
        ,leaf      = 'shape=circle, fillcolor="cornsilk",   style=filled'

      associate(modules => &
        dag_t([ &
          vertex_t(assert_m, [integer::],          names(assert_m), var_str(external_)) &
         ,vertex_t(vertex_m, [integer::],          names(vertex_m), var_str(leaf)) &
         ,vertex_t(vertex_s, [vertex_m, assert_m], names(vertex_s), var_str(root)) &
         ,vertex_t(dag_m,    [vertex_m],           names(dag_m),    var_str(branch)) &
         ,vertex_t(dag_s,    [dag_m, assert_m],    names(dag_s),    var_str(root)) &
        ]))
        block 
          character(len=*), parameter :: base_file_name = 'module-dependencies'
          character(len=*), parameter :: dot_file_name  = base_file_name // '.dot'
          character(len=*), parameter :: pdf_file_name  = base_file_name // '.pdf'
          character(len=*), parameter :: command = 'dot -Tpdf -o ' // pdf_file_name // ' ' // dot_file_name
          integer, parameter :: success=0
          integer exit_status, command_status

          call modules%save_digraph(dot_file_name, 'RL', 300)
          call execute_command_line(command, wait=.true., exitstat=exit_status, cmdstat=command_status)

          result_ =  assert_equals(success, exit_status) .and. assert_equals(success, command_status) 
        end block
      end associate
    end block

  end function

end module dag_test
