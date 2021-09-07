program labeled_dag_output
  use dag_interface, only : dag_t
  use vertex_interface, only : vertex_t
  use iso_varying_string, only : var_str, varying_string
  implicit none

  enum, bind(C)
    enumerator :: &
    app_m=1, dag_m, task_item_m, compile_m, app_generator_m, data_loc_map_m, image_m, main, &
    task_item_s, compile_s, app_generator_s, data_loc_map_s, image_s
  end enum
  integer, parameter ::  num_vertices = size([ &
    app_m,   dag_m, task_item_m, compile_m, app_generator_m, data_loc_map_m, image_m, main, &
    task_item_s, compile_s, app_generator_s, data_loc_map_s, image_s ])

  type(varying_string) names(num_vertices)

  names(app_m) = var_str("app_m")
  names(dag_m) = var_str("dag_m")
  names(task_item_m) = var_str("task_item_m")
  names(compile_m) = var_str("compile_m")
  names(app_generator_m) = var_str("app_generator_m")
  names(data_loc_map_m) = var_str("data_loc_map_m")
  names(image_m) = var_str("image_m")
  names(main) = var_str("main")
  names(task_item_s) = var_str("task_item_s")
  names(compile_s) = var_str("compile_s")
  names(app_generator_s) = var_str("app_generator_s")
  names(data_loc_map_s) = var_str("data_loc_map_s")
  names(image_s) = var_str("image_s")
    
  block
    character(len=*),                   parameter :: non_leaf_color = 'shape=square,fillcolor="SlateGray1",style=filled'
    character(len=len(non_leaf_color)), parameter :: leaf_color     = 'shape=circle,fillcolor="cornsilk",style=filled'
    character(len=*), parameter :: base_name= 'feats-dependencies'
    character(len=*), parameter :: digraph_file = base_name // '.dot'
    character(len=*), parameter :: output_file = base_name // '.pdf'
    
    associate(feats => &
      dag_t([ &
        vertex_t(app_m, [integer:: ], names(app_m), var_str(leaf_color)), &
        vertex_t(dag_m, [integer:: ], names(dag_m), var_str(leaf_color)), &
        vertex_t(task_item_m, [integer:: ], names(task_item_m), var_str(leaf_color)), &
        vertex_t(compile_m, [integer:: ], names(compile_m), var_str(leaf_color)), &
        vertex_t(app_generator_m, [app_m, dag_m, task_item_m, compile_m], names(app_generator_m), var_str(non_leaf_color)), &
        vertex_t(data_loc_map_m, [integer::], names(data_loc_map_m), var_str(leaf_color)), &
        vertex_t(image_m, [app_m, data_loc_map_m], names(image_m), var_str(non_leaf_color)), &
        vertex_t(main, [app_generator_m, image_m], names(main), var_str(non_leaf_color)), &
        vertex_t(task_item_s, [task_item_m], names(task_item_s), var_str(leaf_color)), &
        vertex_t(compile_s, [compile_m], names(compile_s), var_str(leaf_color)), &
        vertex_t(app_generator_s, [app_generator_m], names(app_generator_s), var_str(leaf_color)), &
        vertex_t(data_loc_map_s, [data_loc_map_m], names(data_loc_map_s), var_str(leaf_color)), &
        vertex_t(image_s, [image_m], names(image_s), var_str(leaf_color)) &
      ]))
      call feats%save_digraph(digraph_file, 'RL', 300)
      call execute_command_line('dot -Tpdf -o ' // output_file // ' ' // digraph_file)
    print *, new_line(''), " main: module_depenencies DAG written to " // output_file
    end associate
  end block

end program
