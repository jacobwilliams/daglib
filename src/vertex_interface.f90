!*******************************************************************************
!>
!  DAG Module.

    module vertex_interface

    implicit none

    private

    type :: vertex
        !! a vertex of a directed acyclic graph (DAG)
        private
        integer,dimension(:),allocatable :: edges  !! these are the vertices that this vertex depends on
        integer :: ivertex = 0 !! vertex number
        logical :: checking = .false.  !! used for toposort
        logical :: marked = .false.    !! used for toposort
        character(len=:),allocatable :: label      !! used for diagraph
        character(len=:),allocatable :: attributes !! used for diagraph
    contains
        generic :: set_edges => set_edge_vector, add_edge
        procedure :: set_edge_vector
        procedure :: add_edge
    end type vertex

contains

!*******************************************************************************
!>
!  specify the edge indices for this vertex

    subroutine set_edge_vector(me,edges)

    class(vertex),intent(inout)     :: me
    integer,dimension(:),intent(in) :: edges

    integer :: i !! counter

    if (allocated(me%edges)) then
        do i=1,size(edges)
            call me%add_edge(edges(i))
        end do
    else
        allocate(me%edges(size(edges)))  ! note: not checking for uniqueness here.
        me%edges = edges
    end if

    end subroutine set_edge_vector
!*******************************************************************************

!*******************************************************************************
!>
!  add an edge index for this vertex

    subroutine add_edge(me,edge)

    class(vertex),intent(inout) :: me
    integer,intent(in) :: edge

    if (allocated(me%edges)) then
        if (.not. any (edge==me%edges)) then
            me%edges = [me%edges, edge]  ! auto lhs reallocation
        end if
    else
        allocate(me%edges(1))
        me%edges = [edge]
    end if

    end subroutine add_edge
!*******************************************************************************

    end module vertex_interface
!*******************************************************************************
