submodule(vertex_m) vertex_s
  use jsonff, only : &
      fallible_json_string_t, &
      fallible_json_value_t, &
      json_array_t, &
      json_element_t, &
      json_number_t, &
      json_string_t, &
      json_integer_t
  use erloff, only : error_list_t
  use iso_varying_string, only : char, assignment(=)
  use iso_fortran_env, only : real64
  use assert_m, only : assert
  implicit none

contains

  module procedure to_json
    integer i
    type(json_string_t) :: edges_key, label_key, label_value
    type(json_array_t) :: edges_value
    type(error_list_t) :: errors
    type(fallible_json_string_t) :: maybe_key, maybe_value


    maybe_key = fallible_json_string_t("edges")
    errors = maybe_key%errors()
    call assert(.not. errors%has_any(), "vertex%to_json (edges key): .not. errors%has_any()", char(errors%to_string()))
    edges_key = maybe_key%string()
    
    if (allocated(self%edges)) then
      do i = lbound(self%edges, 1), ubound(self%edges, 1)
        call edges_value%append(json_integer_t(self%edges(i)))
      end do
    end if
    
    maybe_key = fallible_json_string_t("label")
    errors = maybe_key%errors()
    call assert(.not. errors%has_any(), "vertex%to_json (label key): .not. errors%has_any()", char(errors%to_string()))
    label_key = maybe_key%string()
        
    maybe_value = fallible_json_string_t(self%get_label())
    errors = maybe_value%errors()
    call assert(.not. errors%has_any(), "vertex%to_json (label value): .not. errors%has_any()", char(errors%to_string()))
    label_value = maybe_value%string()

    
    json_object = json_object_t([label_key, edges_key], [json_element_t(label_value), json_element_t(edges_value)])    

  end procedure

  module procedure construct_from_components

    character(len=*), parameter :: &
       branch    = 'shape=square, fillcolor="SlateGray1", style=filled' &
      ,external_ = 'shape=square, fillcolor="green",      style=filled' &
      ,root      = 'shape=circle, fillcolor="white",      style=filled' &
      ,leaf      = 'shape=circle, fillcolor="cornsilk",   style=filled'

    vertex%identifier_ = identifier
    vertex%edges = edges
    vertex%label = label
    if (present(attributes)) then
      vertex%attributes = char(attributes)
    else
      vertex%attributes = branch
    end if
    vertex%has_label_ = .true.
    vertex%defined_ = .true.
  end procedure

  module procedure from_json
    type(error_list_t) :: errors
    type(fallible_json_value_t) :: maybe_edge
    type(fallible_json_value_t) :: maybe_edges
    integer :: i

    maybe_edges = json_object%get_element("edges")
    errors = maybe_edges%errors()
    call assert(.not. errors%has_any(), "vertex%from_json: .not. errors%has_any()", char(errors%to_string()))
    select type (edges => maybe_edges%value_())
    type is (json_array_t)
      allocate(vertex%edges(edges%length()))
      do i = 1, edges%length()
        maybe_edge = edges%get_element(i)
        errors = maybe_edge%errors()
        call assert(.not. errors%has_any(), "vertex%from_json: .not. errors%has_any()", char(errors%to_string()))
        select type (edge => maybe_edge%value_())
        type is (json_number_t)
          vertex%edges(i) = int(edge%get_value())
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

    if (allocated(self%edges)) then
      do i=1,size(edges)
          call self%add_edge(edges(i))
      end do
    else
      allocate(self%edges(size(edges)))  ! note: not checking for uniqueness here.
      self%edges = edges
    end if

  end procedure

  module procedure set_vertex_id
    self%identifier_ = id
  end procedure

  module procedure set_checking
    self%checking = checking
  end procedure

  module procedure set_marked
    self%marked = marked
  end procedure

  module procedure get_vertex_id
    my_vertex_id = self%identifier_
  end procedure

  module procedure get_edges
    my_edges = self%edges
  end procedure

  module procedure get_checking
    my_checking = self%checking
  end procedure

  module procedure get_marked
    my_marked = self%marked
  end procedure

  module procedure get_label
    my_label = self%label
  end procedure

  module procedure get_attributes
    my_attributes = self%attributes
  end procedure

  module procedure add_edge

    if (allocated(self%edges)) then
      if (.not. any (edge==self%edges)) then
          self%edges = [self%edges, edge]
      end if
    else
      allocate(self%edges(1))
      self%edges = [edge]
    end if

  end procedure

  module procedure has_label
    allocated_label = self%has_label_
  end procedure

  module procedure has_attributes
    allocated_attributes = allocated(self%attributes)
  end procedure

  module procedure read_formatted
    error stop "vertex%read_formatted unimplemented"
  end procedure

  module procedure write_formatted
    write(unit, '(a)') '{ "edges" : ['
    write(unit, '(*(I0,:,","))') self%edges
    write(unit, '(a)') '] }'
  end procedure

end submodule vertex_s
