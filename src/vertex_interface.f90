module vertex_interface
  !! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
  !! version: v1.0
  !! date: 2020-Nov-30
  !! license: Copyright (c) 2020-2021, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
    use jsonff, only : json_object_t
    use iso_varying_string, only : varying_string, len

    implicit none

    private

    public :: vertex_t

    type :: vertex_t
      !! Encapsulate a node in a graph comprised of vertices connected by dependencies (edges)
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
      
      module function from_json(me_json) result(me)
        implicit none
        type(json_object_t), intent(in) :: me_json
        type(vertex_t) :: me
      end function
      
    end interface

    interface

       module subroutine set_edge_vector(me,edges)
         !! Define the vertices on which this vertex depends on
         implicit none
         class(vertex_t),intent(inout)     :: me
         integer,dimension(:),intent(in) :: edges
       end subroutine set_edge_vector

       impure elemental module function to_json(me) result(me_json)
         implicit none
         class(vertex_t), intent(in) :: me
         type(json_object_t) :: me_json
       end function

       module subroutine add_edge(me,edge)
         implicit none
         class(vertex_t),intent(inout) :: me
         integer,intent(in) :: edge
       end subroutine add_edge

       module subroutine set_checking(me,checking)
         !! Set the status of the vertex in the toposort algorithm.  {True, False} => {being checked, not being checked},
         !! where being checked implies there is a circular graph.
         implicit none
         class(vertex_t), intent(inout) :: me
         logical, intent(in) :: checking
       end subroutine

       module subroutine set_marked(me,marked)
         !! Set the status of the vertex in the toposort algorithm.  {True, False} => {sort finished, not finished}.
         implicit none
         class(vertex_t), intent(inout) :: me
         logical, intent(in) :: marked
       end subroutine

       elemental module subroutine set_vertex_id(me,id)
         !! Set the vertex number -- not currently checked for uniqueness or order.
         implicit none
         class(vertex_t),intent(inout) :: me
         integer, intent(in) :: id
       end subroutine set_vertex_id

       elemental module subroutine set_label(me,label)
         implicit none
         class(vertex_t),intent(inout) :: me
         type(varying_string), intent(in) :: label
       end subroutine set_label

       elemental module subroutine set_attributes(me,attributes)
         !! Set the vertex attributes for digraph output.
         implicit none
         class(vertex_t),intent(inout) :: me
         character(len=*), intent(in) :: attributes
       end subroutine set_attributes

       pure module function get_vertex_id(me) result(my_vertex_id)
         !! Get the vertex number associated with the vertex index.
         implicit none
         class(vertex_t), intent(in) :: me
         integer my_vertex_id
       end function get_vertex_id

       pure module function get_edges(me) result(my_edges)
         !! Get the dependent vertices that this vertex depends on
         implicit none
         class(vertex_t), intent(in) :: me
         integer :: my_edges(size(me%edges))
       end function get_edges

       pure module function get_checking(me) result(my_checking)
         !! Get the status of the checking variable.
         implicit none
         class(vertex_t), intent(in) :: me
         logical my_checking
       end function get_checking

       pure module function get_marked(me) result(my_marked)
        !! Get the status of the marked variable.
         implicit none
         class(vertex_t), intent(in) :: me
         logical my_marked
       end function get_marked

       pure module function get_label(me) result(my_label)
         implicit none
         class(vertex_t), intent(in) :: me
         character(len=len(me%label)) my_label
       end function get_label

       pure module function get_attributes(me) result(my_attributes)
         implicit none
         class(vertex_t), intent(in) :: me
         character(len=len(me%attributes)) my_attributes
       end function get_attributes

       pure module function has_label(me) result(allocated_label)
         implicit none
         class(vertex_t), intent(in) :: me
         logical allocated_label
       end function has_label

       pure module function has_attributes(me) result(allocated_attributes)
         implicit none
         class(vertex_t), intent(in) :: me
         logical allocated_attributes
       end function has_attributes

       module subroutine read_formatted(me, unit, iotype, vlist, iostat, iomsg)
         !! Read a vertex from a JSON file
         implicit none
         class(vertex_t), intent(inout) :: me
         integer, intent(in) :: unit
         character (len=*), intent(in) :: iotype
         integer, intent(in) :: vlist(:)
         integer, intent(out) :: iostat
         character (len=*), intent(inout) :: iomsg
       end subroutine read_formatted

       module subroutine write_formatted(me, unit, iotype, vlist, iostat, iomsg)
         !! Write a vertex to a JSON file using Everythingfunctional/jsonff
         implicit none
         class(vertex_t), intent(in) :: me
         integer, intent(in) :: unit
         character (len=*), intent(in) :: iotype
         integer, intent(in) :: vlist(:)
         integer, intent(out) :: iostat
         character (len=*), intent(inout) :: iomsg
       end subroutine write_formatted

    end interface

end module vertex_interface
