!*******************************************************************************
!>
!  DAG Module.

    module vertex_interface

    implicit none

    private
    public :: vertex

    type :: vertex
        !! a vertex of a directed acyclic graph (DAG)
        !private
        integer,dimension(:),allocatable :: edges  !! these are the vertices that this vertex depends on
        integer :: ivertex = 0 !! vertex number
        logical :: checking = .false.  !! used for toposort
        logical :: marked = .false.    !! used for toposort
        character(len=:),allocatable :: label      !! used for diagraph
        character(len=:),allocatable :: attributes !! used for diagraph
    contains
        generic :: set_edges => set_edge_vector, add_edge
        procedure :: set_edge_vector
        procedure :: get_vertex_id
        procedure :: get_checking
        procedure :: get_marked
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
!  result is vertex number

    pure module function get_vertex_id(me) result(my_vertex_id)
    implicit none
    class(vertex), intent(in) :: me
    integer my_vertex_id
    end function
!*******************************************************************************

!*******************************************************************************
!>
!  result is used for toposort

    pure module function get_checking(me) result(my_checking)
    implicit none
    class(vertex), intent(in) :: me
    integer my_checking
    end function
!*******************************************************************************

!*******************************************************************************
!>
!  result is used for toposort

    pure module function get_marked(me) result(my_marked)
    implicit none
    class(vertex), intent(in) :: me
    integer my_marked
    end function
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
