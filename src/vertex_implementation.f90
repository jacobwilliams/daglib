submodule(vertex_interface) vertex_implementation
  implicit none

contains

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
    me%ivertex = id
  end procedure

  module procedure set_checking
    me%checking = checking
  end procedure

  module procedure set_marked
    me%marked = marked
  end procedure

  module procedure set_label
    me%label = label
  end procedure

  module procedure set_attributes
    me%attributes = attributes
  end procedure

  module procedure get_vertex_id
    my_vertex_id = me%ivertex
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
    allocated_label = allocated(me%label)
  end procedure

  module procedure has_attributes
    allocated_attributes = allocated(me%attributes)
  end procedure

  module procedure write_formatted
    write(unit,'(a,(*(G0:,",")),a)') '{ "edges" : [', this%edges, '] }'
  end procedure

end submodule
