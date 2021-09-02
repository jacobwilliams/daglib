program labeled_dag_output
  use dag_interface, only : dag_t
  implicit none

  type module_name_t
    character(len=:), allocatable :: module_name
  end type

  type(module_name_t), allocatable :: name_list(:)
  type(dag_t) :: module_dependencies

  enum, bind(C)
    ! a topologically sorted enumeration of the items in the dependency tree
    enumerator :: &
    build_feats=1, application_generator_m, image_m, data_location_map_m, application_m, dag_interface, task_item_m, & 
    payload_m, task_m, mailbox_m, assertions_interface, application_s, payload_s, data_location_map_s, image_s, task_item_s
  end enum

  integer, parameter :: vertices(*) = [ &
    build_feats,   application_generator_m, image_m, data_location_map_m, application_m, dag_interface, task_item_m, & 
    payload_m, task_m, mailbox_m, assertions_interface, application_s, payload_s, data_location_map_s, image_s, task_item_s &
  ]
  integer, parameter :: num_vertices = size(vertices)

  allocate(name_list(num_vertices))

  name_list(build_feats)             = module_name_t("build_feats")
  name_list(application_generator_m) = module_name_t("application_generator_m")
  name_list(image_m                ) = module_name_t("image_m")
  name_list(data_location_map_m    ) = module_name_t("data_location_map_m")
  name_list(application_m          ) = module_name_t("application_m")
  name_list(dag_interface          ) = module_name_t("dag_interface")
  name_list(task_item_m            ) = module_name_t("task_item_m")
  name_list(payload_m              ) = module_name_t("payload_m")
  name_list(task_m                 ) = module_name_t("task_m")
  name_list(mailbox_m              ) = module_name_t("mailbox_m")
  name_list(assertions_interface   ) = module_name_t("assertions_interface")
  name_list(application_s          ) = module_name_t("application_s")
  name_list(payload_s              ) = module_name_t("payload_s")
  name_list(data_location_map_s    ) = module_name_t("data_location_map_s")
  name_list(image_s                ) = module_name_t("image_s")
  name_list(task_item_s            ) = module_name_t("task_item_s")

  call module_dependencies%set_vertices(num_vertices)
  block 
    integer i

    do i = 1, num_vertices
      call module_dependencies%set_vertex_info(i, label=name_list(i)%module_name)
    end do

  end block
  call module_dependencies%set_edges(build_feats, [application_generator_m, image_m])   
  call module_dependencies%set_edges(image_m, [data_location_map_m, application_m])
  call module_dependencies%set_edges(application_m, [dag_interface, task_item_m])
  call module_dependencies%set_edges(task_item_m, [data_location_map_m, payload_m, task_m])
  call module_dependencies%set_edges(task_m, [data_location_map_m, payload_m])
  call module_dependencies%set_edges(mailbox_m, [payload_m])
  call module_dependencies%set_edges(application_s, [application_m, assertions_interface])
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
      assertions_interface, application_s, payload_s, data_location_map_s, image_s, task_item_s]
    
    do i = 1, num_vertices
      associate(node_color => merge(leaf_color, non_leaf_color, any(i==leaf_nodes)))
        call module_dependencies%set_vertex_info(i, attributes = node_color)
      end associate
    end do

  end block

  block
    character(len=*), parameter :: base_name= 'feats-dependencies'
    character(len=*), parameter :: digraph_file = base_name // '.dot'
    character(len=*), parameter :: output_file = base_name // '.pdf'

    call module_dependencies%save_digraph(digraph_file, 'RL', 300)
    call execute_command_line('dot -Tpdf -o ' // output_file // ' ' // digraph_file)
    print *, new_line(''), " ----- application_generator(): module_depenencies DAG written to " // output_file
  end block

end program
