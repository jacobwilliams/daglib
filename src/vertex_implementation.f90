!*******************************************************************************
!>
!  DAG Module.

submodule(vertex_interface) vertex_implementation

    implicit none

contains

!*******************************************************************************
!>
!  specify the edge indices for this vertex

    module procedure set_edge_vector

    integer :: i !! counter

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

!*******************************************************************************
!>
!  add an edge index for this vertex

    module procedure add_edge

    if (allocated(me%edges)) then
        if (.not. any (edge==me%edges)) then
            me%edges = [me%edges, edge]  ! auto lhs reallocation
        end if
    else
        allocate(me%edges(1))
        me%edges = [edge]
    end if

    end procedure
!*******************************************************************************

    end submodule
!*******************************************************************************
