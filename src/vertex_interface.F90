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
#ifdef __GFORTRAN__
        integer, public, allocatable :: edges(:) !! vertices on which this vertex depends
#else
        integer,         allocatable :: edges(:) !! vertices on which this vertex depends
#endif
        integer :: ivertex = 0 !! vertex identifier
        logical :: checking = .false., marked = .false. !! used for toposort
        character(len=:), allocatable :: label, attributes !! used for diagraph
    contains
        generic :: set_edges => set_edge_vector, add_edge
        procedure :: set_edge_vector
        procedure :: set_checking
        procedure :: set_marked
        procedure :: set_vertex_id
        procedure :: set_label
        procedure :: set_attributes
        procedure :: get_vertex_id
        procedure :: get_checking
        procedure :: get_marked
        procedure :: get_label
        procedure :: get_attributes
        procedure :: add_edge
        procedure :: has_label
        procedure :: has_attributes
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
!  define checking for use in toposort

    module subroutine set_checking(me,checking)
    implicit none
    class(vertex), intent(inout) :: me
    logical, intent(in) :: checking
    end subroutine
!*******************************************************************************

!*******************************************************************************
!>
!  define checking for use in toposort

    module subroutine set_marked(me,marked)
    implicit none
    class(vertex), intent(inout) :: me
    logical, intent(in) :: marked
    end subroutine
!*******************************************************************************

!*******************************************************************************
!>
!  define the vertex identifiers

    elemental module subroutine set_vertex_id(me,id)
    implicit none
    class(vertex),intent(inout) :: me
    integer, intent(in) :: id
    end subroutine
!*******************************************************************************

!*******************************************************************************
!>
!  define the vertex labels

    elemental module subroutine set_label(me,label)
    implicit none
    class(vertex),intent(inout) :: me
    character(len=*), intent(in) :: label
    end subroutine
!*******************************************************************************

!*******************************************************************************
!>
!  define the vertex attributes

    elemental module subroutine set_attributes(me,attributes)
    implicit none
    class(vertex),intent(inout) :: me
    character(len=*), intent(in) :: attributes
    end subroutine
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
    logical my_checking
    end function
!*******************************************************************************

!*******************************************************************************
!>
!  result is used for toposort

    pure module function get_marked(me) result(my_marked)
    implicit none
    class(vertex), intent(in) :: me
    logical my_marked
    end function
!*******************************************************************************

!*******************************************************************************
!>
!  result is label for this vertex

    pure module function get_label(me) result(my_label)
    implicit none
    class(vertex), intent(in) :: me
    character(len=len(me%label)) my_label
    end function
!*******************************************************************************

!*******************************************************************************
!>
!  result contains the attributes for this vertex

    pure module function get_attributes(me) result(my_attributes)
    implicit none
    class(vertex), intent(in) :: me
    character(len=len(me%attributes)) my_attributes
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

!*******************************************************************************
!>
!  result is true if label is allocated

    pure module function has_label(me) result(allocated_label)
    implicit none
    class(vertex), intent(in) :: me
    logical allocated_label
    end function
!*******************************************************************************

!*******************************************************************************
!>
!  result is true if attributes is allocated

    pure module function has_attributes(me) result(allocated_attributes)
    implicit none
    class(vertex), intent(in) :: me
    logical allocated_attributes
    end function
!*******************************************************************************


end interface

    end module vertex_interface
!*******************************************************************************
