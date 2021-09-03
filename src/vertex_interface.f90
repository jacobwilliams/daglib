module vertex_interface
    use jsonff, only : json_object_t
    use iso_varying_string, only : varying_string, len

    implicit none

    private

    public :: vertex_t

    type :: vertex_t
!! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
!! version: v1.0
!! date: 2020-Nov-30
!! license: Copyright (c) 2020, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
!!<hr />
!! Vertex Module
!!<hr />
!! Defines how to represent a vertex in graph theory <https://en.wikipedia.org/wiki/Vertex_(graph_theory)>.  This is done with an array of edges which lists the vertices that this vertex depends on.  Also a vertex number is needed to identify which vertex this instantiation represents.  Labels and attributes are also included for the vertex for digraph output.  There are two logical variables needed by the sort algorithm to place these task in the proper order.
!!
!! Public data: edges, an allocatable, rank 1 integer array. Note: a GCC bug is keeping this data from being private, when the bug is fixed, this data will become private and will affect routines:
!!
!!   - dag_get_edges
!!   - dag_get_dependencies
!!   - dag_toposort/dfs
!!   - dag_generate_digraph
!!   - dag_generate_dependency_matrix
!!
!! Private data: label and attributes, allocatable length, scalar character arrays for digraph output.  checking and marked, scalar logicals with defaults set to .false. and used by toposort.
!!
!!<hr />
!! Public Vertex Interfaces:
!!
!!    me(Vertex Index)%set_edges ( Array or Scalar of dependent vertex number(s) )
!!
!!        - An array or a scalar list of other vertices that this vertex depends upon
!!
!!    edges = me(Vertex Index)%get_edges ( )
!!
!!        - Get the dependent vertices that this vertex depends on
!!
!!    me(Vertex Index)%set_checking ( .true. or .false. )
!!
!!        - Set the status of the vertex in the toposort algorithm.  True means being checked.  False means not being checked.  If a vertex is being checked (mean this value is true), then there is a circular graph.
!!
!!    check = me(Vertex Index)%get_checking ( )
!!
!!        - Get the status of the checking variable.  Logical value.
!!
!!    me(Vertex Index)%set_marked ( .true. or .false. )
!!
!!        - Set the status of the vertex in the toposort algorithm.  True means sort is finished.  False means not finished.
!!
!!    mark = me(Vertex Index)%get_marked ( )
!!
!!        - Get the status of the marked variable.  Logical value.
!!
!!    me(Vertex Index)%set_vertex_id ( Vertex ID ) or me%set_vertex_id ( Vertex ID Array )
!!
!!        - Set the vertex number to an integer number.  This is not checked for uniqueness or order.
!!
!!    id = me(Vertex Index)%get_vertex_id ( )
!!
!!        - Get the vertex number associated with the vertex index.
!!
!!    me(Vertex Index)%set_label ( Label )
!!
!!        - Set the vertex label to be Label for digraph output.  Automatic allocation to fit the size of Label.
!!
!!    label = me(Vertex Index)%get_label ( )
!!
!!        - Get the label value for the vertex index for digraph output.  The result is sized to the label being read.
!!
!!    me(Vertex Index)%set_attributes ( Attributes )
!!
!!        - Set the vertex attributes to be Attributes for digraph output.  Automatic allocation to fit the size of Attributes.
!!
!!    attributes = me(Vertex Index)%get_attributes ( )
!!
!!        - Get the attributes value for the vertex index for digraph output.  The result is sized to the attributes being read.
!!
!!<hr />
!! Private Vertex Interfaces:
!!
!!    me%set_edge_vector ( Array of Edges )
!!
!!        - An array of edges, other vertices, that this vertex depends on
!!
!!    me%add_edge ( Scalar Edge )
!!
!!        - A scalar value if this vertex only depends on 1 other vertex
!!
!!<hr />
!! Read and Write Formatted Input/Output:
!!
!!    read_formatted ()
!!
!!       - Read a vertex from a JSON file using Everythingfunctional/jsonff
!!
!!    write_formatted ()
!!
!!       - Write a vertex to a JSON file using Everythingfunctional/jsonff
!!
!!<hr />
!! Call Order to ...
!!<hr />
!
        private
        integer, public, allocatable  :: edges(:)
!        integer, allocatable          :: edges(:)  -  should be private, but a bug in GCC needs it to be public.  When fixed, this will be private again
        integer                       :: ivertex = 0
        logical                       :: checking = .false., marked = .false., has_label_ = .false.
        type(varying_string)          :: label
        character(len=:), allocatable :: attributes
    contains
        procedure, public  :: to_json
        procedure, private ::              set_edge_vector, add_edge
        generic,   public  :: set_edges => set_edge_vector, add_edge
        procedure, public  :: get_edges

        procedure, public  :: set_checking,   get_checking
        procedure, public  :: set_marked,     get_marked
        procedure, public  :: set_vertex_id,  get_vertex_id
        procedure, public  :: set_label
        procedure, public  :: get_label,      has_label
        procedure, public  :: set_attributes, get_attributes, has_attributes

        procedure, private :: read_formatted
        generic,   public  :: read(formatted) => read_formatted
        procedure, private :: write_formatted
        generic,   public  :: write(formatted) => write_formatted
    end type vertex_t

    interface vertex_t
      procedure from_json
    end interface

    interface

!*******************************************************************************
       module subroutine set_edge_vector(me,edges)
         implicit none
         class(vertex_t),intent(inout)     :: me
         integer,dimension(:),intent(in) :: edges
       end subroutine set_edge_vector
!*******************************************************************************
       impure elemental module function to_json(me) result(me_json)
         implicit none
         class(vertex_t), intent(in) :: me
         type(json_object_t) :: me_json
       end function
!*******************************************************************************
       module function from_json(me_json) result(me)
         implicit none
         type(json_object_t), intent(in) :: me_json
         type(vertex_t) :: me
       end function
!*******************************************************************************
       module subroutine add_edge(me,edge)
         implicit none
         class(vertex_t),intent(inout) :: me
         integer,intent(in) :: edge
       end subroutine add_edge
!*******************************************************************************
       module subroutine set_checking(me,checking)
         implicit none
         class(vertex_t), intent(inout) :: me
         logical, intent(in) :: checking
       end subroutine
!*******************************************************************************
       module subroutine set_marked(me,marked)
         implicit none
         class(vertex_t), intent(inout) :: me
         logical, intent(in) :: marked
       end subroutine
!*******************************************************************************
       elemental module subroutine set_vertex_id(me,id)
         implicit none
         class(vertex_t),intent(inout) :: me
         integer, intent(in) :: id
       end subroutine set_vertex_id
!*******************************************************************************
       elemental module subroutine set_label(me,label)
         implicit none
         class(vertex_t),intent(inout) :: me
         type(varying_string), intent(in) :: label
       end subroutine set_label
!*******************************************************************************
       elemental module subroutine set_attributes(me,attributes)
         implicit none
         class(vertex_t),intent(inout) :: me
         character(len=*), intent(in) :: attributes
       end subroutine set_attributes
!*******************************************************************************
       pure module function get_vertex_id(me) result(my_vertex_id)
         implicit none
         class(vertex_t), intent(in) :: me
         integer my_vertex_id
       end function get_vertex_id
!*******************************************************************************
       pure module function get_edges(me) result(my_edges)
         implicit none
         class(vertex_t), intent(in) :: me
         integer :: my_edges(size(me%edges))
       end function get_edges
!*******************************************************************************
       pure module function get_checking(me) result(my_checking)
         implicit none
         class(vertex_t), intent(in) :: me
         logical my_checking
       end function get_checking
!*******************************************************************************
       pure module function get_marked(me) result(my_marked)
         implicit none
         class(vertex_t), intent(in) :: me
         logical my_marked
       end function get_marked
!*******************************************************************************
       pure module function get_label(me) result(my_label)
         implicit none
         class(vertex_t), intent(in) :: me
         character(len=len(me%label)) my_label
       end function get_label
!*******************************************************************************
       pure module function get_attributes(me) result(my_attributes)
         implicit none
         class(vertex_t), intent(in) :: me
         character(len=len(me%attributes)) my_attributes
       end function get_attributes
!*******************************************************************************
       pure module function has_label(me) result(allocated_label)
         implicit none
         class(vertex_t), intent(in) :: me
         logical allocated_label
       end function has_label
!*******************************************************************************
       pure module function has_attributes(me) result(allocated_attributes)
         implicit none
         class(vertex_t), intent(in) :: me
         logical allocated_attributes
       end function has_attributes
!*******************************************************************************
       module subroutine read_formatted(me, unit, iotype, vlist, iostat, iomsg)
         class(vertex_t), intent(inout) :: me
         integer, intent(in) :: unit
         character (len=*), intent(in) :: iotype
         integer, intent(in) :: vlist(:)
         integer, intent(out) :: iostat
         character (len=*), intent(inout) :: iomsg
       end subroutine read_formatted
!*******************************************************************************
       module subroutine write_formatted(me, unit, iotype, vlist, iostat, iomsg)
         class(vertex_t), intent(in) :: me
         integer, intent(in) :: unit
         character (len=*), intent(in) :: iotype
         integer, intent(in) :: vlist(:)
         integer, intent(out) :: iostat
         character (len=*), intent(inout) :: iomsg
       end subroutine write_formatted
!*******************************************************************************

    end interface

end module vertex_interface
