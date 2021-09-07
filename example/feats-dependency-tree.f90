program labeled_dag_output
  use dag_interface, only : dag_t
  use vertex_interface, only : vertex_t
  use iso_varying_string, only : varying_string, var_str, assignment(=)
  implicit none

  enum, bind(C)
    ! a topologically sorted enumeration of the items in the dependency tree
    enumerator :: &
    main=1, app_generator_m, image_m, app_m, dag_m, task_m, compile_m, data_loc_map_m, task_item_m, & 
    payload_m, mailbox_m, assert_m, app_s, payload_s, data_loc_map_s, image_s, compile_s, task_item_s
  end enum

  integer, parameter :: vertices(*) = [ &
    main, app_generator_m, image_m, app_m, dag_m, task_m, compile_m, data_loc_map_m, task_item_m, & 
    payload_m, mailbox_m, assert_m, app_s, payload_s, data_loc_map_s, image_s, compile_s, task_item_s &
  ]
  integer, parameter :: num_vertices = size(vertices)
  type(varying_string) :: names(num_vertices)

  names(main           ) = "main"
  names(app_generator_m) = "app_generator_m"
  names(image_m        ) = "image_m"
  names(app_m          ) = "app_m"
  names(dag_m          ) = "dag_m"
  names(task_m         ) = "task_m"
  names(compile_m)       = "compile_m"
  names(data_loc_map_m ) = "data_loc_map_m"
  names(task_item_m    ) = "task_item_m"
  names(payload_m      ) = "payload_m"
  names(mailbox_m      ) = "mailbox_m"
  names(assert_m       ) = "assert_m"
  names(app_s          ) = "app_s"
  names(payload_s      ) = "payload_s"
  names(data_loc_map_s ) = "data_loc_map_s"
  names(image_s        ) = "image_s"
  names(compile_s)       = "compile_s"
  names(task_item_s    ) = "task_item_s"

  block
    integer i
    character(len=*),                   parameter :: non_leaf_color = 'shape=square,fillcolor="SlateGray1",style=filled'
    character(len=len(non_leaf_color)), parameter :: leaf_color     = 'shape=circle,fillcolor="cornsilk",style=filled'
    integer, parameter :: leaf_nodes(*) = &
      [app_generator_m, data_loc_map_m, dag_m, payload_m, &
      assert_m, app_s, payload_s, data_loc_map_s, image_s, task_item_s]
    character(len=*), parameter :: base_name= 'feats-dependencies'
    character(len=*), parameter :: digraph_file = base_name // '.dot'
    character(len=*), parameter :: output_file = base_name // '.pdf'
    
    associate(feats => &
      dag_t([ &
        vertex_t(main, [app_generator_m, image_m], names(main), var_str(leaf_color)), &
        vertex_t(app_generator_m, [app_m, dag_m, task_item_m, compile_m], names(app_generator_m), var_str(non_leaf_color)), &
        vertex_t(image_m, [app_m, data_loc_map_m], names(image_m), var_str(non_leaf_color)) &
      ]))
      call feats%save_digraph(digraph_file, 'RL', 300)
      call execute_command_line('dot -Tpdf -o ' // output_file // ' ' // digraph_file)
    print *, new_line(''), " main: module_depenencies DAG written to " // output_file
    end associate
  end block

end program
