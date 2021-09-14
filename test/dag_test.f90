module dag_test
  use dag_m, only: dag_t
  use vertex_m, only: vertex_t
  use vegetables, only: &
      result_t, test_item_t, assert_equals, describe, it, assert_that
  use iso_varying_string, only : varying_string, var_str, assignment(=), char
  use jsonff, only: json_object_t
  !!  Test DAG construction, input, and output.
  implicit none
  private
  public :: test_dag_construction

contains

  function test_dag_construction() result(tests)
    type(test_item_t) :: tests

    tests = describe("dag's module dependency graph", &
      [it("can be constructed, output to .dot file, and converted to a PDF", construct_dag_and_write_pdf) &
      ,it("can be constructed and converted to a JSON object", construct_dag_and_json_object) &
      ,it("is topologically sorted when constructed from components", component_constructor_sorts)])! &
      !,it("is topologically sorted when constructed from a JSON object", json_constructor_sorts)])

  end function

  function module_tree_from_components() result(dag_modules)
    type(dag_t) dag_modules
    
    enum, bind(C)
      enumerator :: assert_m=1, vertex_m, vertex_s, dag_m, dag_s 
    end enum
    
    integer, parameter :: module_id(*) = [assert_m, vertex_m, vertex_s, dag_m, dag_s]
    type(varying_string) :: names(size(module_id))
 
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

      dag_modules = dag_t([ &
         vertex_t(assert_m, [integer::],          names(assert_m), var_str(external_)) &
        ,vertex_t(vertex_m, [integer::],          names(vertex_m), var_str(leaf)) &
        ,vertex_t(vertex_s, [vertex_m, assert_m], names(vertex_s), var_str(root)) &
        ,vertex_t(dag_m,    [vertex_m],           names(dag_m),    var_str(branch)) &
        ,vertex_t(dag_s,    [dag_m, assert_m],    names(dag_s),    var_str(root))])
    end block

  end function

  function construct_dag_and_write_pdf() result(result_)
    type(result_t) result_
    character(len=*), parameter :: base_file_name = 'module-dependencies'
    character(len=*), parameter :: dot_file_name  = base_file_name // '.dot'
    character(len=*), parameter :: pdf_file_name  = base_file_name // '.pdf'
    character(len=*), parameter :: command = 'dot -Tpdf -o ' // pdf_file_name // ' ' // dot_file_name
    integer, parameter :: success=0
    integer exit_status, command_status

    associate(modules => module_tree_from_components())
      call modules%save_digraph(dot_file_name, 'RL', 300)
      call execute_command_line(command, wait=.true., exitstat=exit_status, cmdstat=command_status)
      result_ = assert_equals(success, exit_status) .and. assert_equals(success, command_status) 
    end associate

  end function

  function construct_dag_and_json_object() result(result_)
    type(result_t) result_
    type(json_object_t) json_object
    character(len=*), parameter :: expected_json = &
       '{"vertices":[' // &
         '{"label":"assert_m","edges":[]},' // &
         '{"label":"vertex_m","edges":[]},' // &
         '{"label":"vertex_s","edges":[2,1]},' // &
         '{"label":"dag_m","edges":[2]},' // &
         '{"label":"dag_s","edges":[4,1]}]}'

    associate(dag => module_tree_from_components())
      json_object = dag%to_json()
      result_ = assert_equals(var_str(expected_json), json_object%to_compact_string())
    end associate

  end function

  function component_constructor_sorts() result(result_)
    type(result_t) result_
    
    associate(dag => module_tree_from_components())
      result_ = assert_that(dag%is_sorted_and_acyclic())
    end associate
  end function 

  function json_constructor_sorts() result(result_)
    type(result_t) result_
    type(dag_t) dag 
    character(len=*), parameter :: dag_library_module_dependencies= &
       '{"vertices":[' // &
         '{"label":"assert_m","edges":[]},' // &
         '{"label":"vertex_m","edges":[]},' // &
         '{"label":"vertex_s","edges":[2,1]},' // &
         '{"label":"dag_m","edges":[2]},' // &
         '{"label":"dag_s","edges":[4,1]}]}'
    character(len=len(dag_library_module_dependencies)) json
    
    json = dag_library_module_dependencies
    
    read(json,*) dag 
    result_ = assert_that(dag%is_sorted_and_acyclic())
  end function

end module dag_test
