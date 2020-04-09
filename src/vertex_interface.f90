!*******************************************************************************
!>
!  DAG Module.

    module vertex_interface

    implicit none

    private
    public :: vertex

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

interface

!*******************************************************************************
!>
!  specify the edge indices for this vertex

    module subroutine set_edge_vector(me,edges)
    implicit none
    class(vertex),intent(inout)     :: me
    integer,dimension(:),intent(in) :: edges
    end subroutine set_edge_vector
!*******************************************************************************

!*******************************************************************************
!>
!  add an edge index for this vertex

    module subroutine add_edge(me,edge)
    implicit none
    class(vertex),intent(inout) :: me
    integer,intent(in) :: edge
    end subroutine add_edge
!*******************************************************************************

end interface

    end module vertex_interface
!*******************************************************************************
