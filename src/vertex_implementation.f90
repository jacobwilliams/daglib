submodule(vertex_interface) vertex_implementation
  use jsonff, only : &
      fallible_json_string_t, &
      fallible_json_value_t, &
      json_array_t, &
      json_element_t, &
      json_number_t, &
      json_string_t
  use erloff, only : error_list_t
  use iso_varying_string, only : char, assignment(=)
  use iso_fortran_env, only : real64
  use assert_m, only : assert
  implicit none

contains

  module procedure to_json
    integer i
    type(json_string_t) :: edges_key
    type(json_array_t) :: edges_value
    type(error_list_t) :: errors
    type(fallible_json_string_t) :: maybe_key

    if (allocated(me%edges)) then
      do i = lbound(me%edges, 1), ubound(me%edges, 1)
        call edges_value%append(json_number_t(real(me%edges(i), real64)))
      end do
    end if
    maybe_key = fallible_json_string_t("edges")
    errors = maybe_key%errors()
    call assert(.not. errors%has_any(), "vertex%to_json: .not. errors%has_any()", char(errors%to_string()))
    edges_key = maybe_key%string()
    me_json = json_object_t([edges_key], [json_element_t(edges_value)])
  end procedure

  module procedure construct
    new_vertex%identifier_ = identifier
    new_vertex%edges = edges
    new_vertex%label = label
    new_vertex%attributes = char(attributes)
    new_vertex%has_label_ = .true.
    new_vertex%defined_ = .true.
  end procedure

  module procedure from_json
    type(error_list_t) :: errors
    type(fallible_json_value_t) :: maybe_edge
    type(fallible_json_value_t) :: maybe_edges
    integer :: i

    maybe_edges = me_json%get_element("edges")
    errors = maybe_edges%errors()
    call assert(.not. errors%has_any(), "vertex%from_json: .not. errors%has_any()", char(errors%to_string()))
    select type (edges => maybe_edges%value_())
    type is (json_array_t)
      allocate(me%edges(edges%length()))
      do i = 1, edges%length()
        maybe_edge = edges%get_element(i)
        errors = maybe_edge%errors()
        call assert(.not. errors%has_any(), "vertex%from_json: .not. errors%has_any()", char(errors%to_string()))
        select type (edge => maybe_edge%value_())
        type is (json_number_t)
          me%edges(i) = int(edge%get_value())
        class default
          call assert(.false., "vertex%from_json: edge was not a number", char(edge%to_compact_string()))
        end select
      end do
    class default
      call assert(.false., "vertex%from_json: edges was not an array", char(edges%to_compact_string()))
    end select
  end procedure

  module procedure set_edge_vector

    integer i

    if (allocated(me%edges)) then
      do i=1,size(edges)
          call me%add_edge(edges(i))
      end do
    else
      allocate(me%edges(size(edges)))  ! note: not checking for uniqueness here.
      me%edges = edges
    end if

  end procedure

  module procedure set_vertex_id
    me%identifier_ = id
  end procedure

  module procedure set_checking
    me%checking = checking
  end procedure

  module procedure set_marked
    me%marked = marked
  end procedure

  module procedure set_label
    me%label = label
    me%has_label_ = .true.
  end procedure

  module procedure set_attributes
    me%attributes = attributes
  end procedure

  module procedure get_vertex_id
    my_vertex_id = me%identifier_
  end procedure

  module procedure get_edges
    my_edges = me%edges
  end procedure

  module procedure get_checking
    my_checking = me%checking
  end procedure

  module procedure get_marked
    my_marked = me%marked
  end procedure

  module procedure get_label
    my_label = me%label
  end procedure

  module procedure get_attributes
    my_attributes = me%attributes
  end procedure

  module procedure add_edge

    if (allocated(me%edges)) then
      if (.not. any (edge==me%edges)) then
          me%edges = [me%edges, edge]
      end if
    else
      allocate(me%edges(1))
      me%edges = [edge]
    end if

  end procedure

  module procedure has_label
    allocated_label = me%has_label_
  end procedure

  module procedure has_attributes
    allocated_attributes = allocated(me%attributes)
  end procedure

  module procedure read_formatted
    error stop "vertex%read_formatted unimplemented"
  end procedure

  module procedure write_formatted
    write(unit, '(a)') '{ "edges" : ['
    write(unit, '(*(I0,:,","))') me%edges
    write(unit, '(a)') '] }'
  end procedure

end submodule
