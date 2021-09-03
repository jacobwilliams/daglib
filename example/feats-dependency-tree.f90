program labeled_dag_output
  use dag_interface, only : dag_t
  use iso_varying_string, only : varying_string, var_str, assignment(=)
  implicit none

  enum, bind(C)
    ! a topologically sorted enumeration of the items in the dependency tree
    enumerator :: &
    build_feats=1, application_generator_m, image_m, data_location_map_m, application_m, dag_interface, task_item_m, & 
    payload_m, task_m, mailbox_m, assert_m, application_s, payload_s, data_location_map_s, image_s, task_item_s
  end enum

  integer, parameter :: vertices(*) = [ &
    build_feats,   application_generator_m, image_m, data_location_map_m, application_m, dag_interface, task_item_m, & 
    payload_m, task_m, mailbox_m, assert_m, application_s, payload_s, data_location_map_s, image_s, task_item_s &
  ]
  integer, parameter :: num_vertices = size(vertices)
  type(varying_string) :: name_list(num_vertices)
  type(dag_t) :: module_dependencies

  name_list(build_feats)             = "build_feats"
  name_list(application_generator_m) = "application_generator_m"
  name_list(image_m                ) = "image_m"
  name_list(data_location_map_m    ) = "data_location_map_m"
  name_list(application_m          ) = "application_m"
  name_list(dag_interface          ) = "dag_interface"
  name_list(task_item_m            ) = "task_item_m"
  name_list(payload_m              ) = "payload_m"
  name_list(task_m                 ) = "task_m"
  name_list(mailbox_m              ) = "mailbox_m"
  name_list(assert_m               ) = "assert_m"
  name_list(application_s          ) = "application_s"
  name_list(payload_s              ) = "payload_s"
  name_list(data_location_map_s    ) = "data_location_map_s"
  name_list(image_s                ) = "image_s"
  name_list(task_item_s            ) = "task_item_s"

  call module_dependencies%set_vertices(num_vertices)
  call module_dependencies%set_vertex_label(vertices, name_list)

  call module_dependencies%set_edges(build_feats, [application_generator_m, image_m])   
  call module_dependencies%set_edges(image_m, [data_location_map_m, application_m])
  call module_dependencies%set_edges(application_m, [dag_interface, task_item_m])
  call module_dependencies%set_edges(task_item_m, [data_location_map_m, payload_m, task_m])
  call module_dependencies%set_edges(task_m, [data_location_map_m, payload_m])
  call module_dependencies%set_edges(mailbox_m, [payload_m])
  call module_dependencies%set_edges(application_s, [application_m, assert_m])
  call module_dependencies%set_edges(payload_s, [payload_m])
  call module_dependencies%set_edges(data_location_map_s, [data_location_map_m])
  call module_dependencies%set_edges(image_s, [image_m])
  call module_dependencies%set_edges(task_item_s, [task_item_m])
 
  block
    integer i
    character(len=*),                   parameter :: non_leaf_color = 'shape=square,fillcolor="SlateGray1",style=filled'
    character(len=len(non_leaf_color)), parameter :: leaf_color     = 'shape=circle,fillcolor="cornsilk",style=filled'
    integer, parameter :: leaf_nodes(*) = &
      [application_generator_m, data_location_map_m, dag_interface, payload_m, &
      assert_m, application_s, payload_s, data_location_map_s, image_s, task_item_s]
    
    do i = 1, num_vertices
      associate(node_color => merge(leaf_color, non_leaf_color, any(i==leaf_nodes)))
        call module_dependencies%set_vertex_attributes(i, node_color)
      end associate
    end do

  end block

  block
    character(len=*), parameter :: base_name= 'feats-dependencies'
    character(len=*), parameter :: digraph_file = base_name // '.dot'
    character(len=*), parameter :: output_file = base_name // '.pdf'

    call module_dependencies%save_digraph(digraph_file, 'RL', 300)
    call execute_command_line('dot -Tpdf -o ' // output_file // ' ' // digraph_file)
    print *, new_line(''), " main: module_depenencies DAG written to " // output_file
  end block

end program
