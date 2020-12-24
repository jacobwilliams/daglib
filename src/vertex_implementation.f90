submodule(vertex_interface) vertex_implementation
  use jsonff, only : JsonArray_t, JsonNumber_t, JsonNumber, JsonArray, JsonString_t, JsonString, JsonElement
  use erloff, only : ErrorList_t
  use iso_varying_string, only : char
  use iso_fortran_env, only : real64
  use assertions_interface, only : assert
  implicit none

contains

!*******************************************************************************
  module procedure to_json
    integer i
    type(JsonString_t) :: edges_key
    type(JsonArray_t) :: edges_value
    type(ErrorList_t) :: errors

    do i= lbound(me%edges, 1), ubound(me%edges, 1)
      call edges_value%append(JsonNumber(real(me%edges(i), real64)))
    end do
    call JsonString("edges", errors, edges_key)
    call assert(.not. errors%hasany(), "vertex%to_json: .not. errors%hasany()", char(errors%toString()))
    call me_json%add(edges_key, edges_value)
  end procedure
!*******************************************************************************

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

!*******************************************************************************

  module procedure set_vertex_id
    me%ivertex = id
  end procedure

!*******************************************************************************

  module procedure set_checking
    me%checking = checking
  end procedure

!*******************************************************************************

  module procedure set_marked
    me%marked = marked
  end procedure

!*******************************************************************************

  module procedure set_label
    me%label = label
  end procedure

!*******************************************************************************

  module procedure set_attributes
    me%attributes = attributes
  end procedure

!*******************************************************************************

  module procedure get_vertex_id
    my_vertex_id = me%ivertex
  end procedure

!*******************************************************************************

  module procedure get_edges
    my_edges = me%edges
  end procedure

!*******************************************************************************

  module procedure get_checking
    my_checking = me%checking
  end procedure

!*******************************************************************************

  module procedure get_marked
    my_marked = me%marked
  end procedure

!*******************************************************************************

  module procedure get_label
    my_label = me%label
  end procedure

!*******************************************************************************

  module procedure get_attributes
    my_attributes = me%attributes
  end procedure

!*******************************************************************************

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

!*******************************************************************************

  module procedure has_label
    allocated_label = allocated(me%label)
  end procedure

!*******************************************************************************

  module procedure has_attributes
    allocated_attributes = allocated(me%attributes)
  end procedure

!*******************************************************************************

  module procedure read_formatted
    error stop "vertex%read_formatted unimplemented"
  end procedure

!*******************************************************************************

  module procedure write_formatted
    write(unit, '(a)') '{ "edges" : ['
    write(unit, '(*(I0,:,","))') me%edges
    write(unit, '(a)') '] }'
  end procedure

!*******************************************************************************

end submodule
