module vertex_m
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
        integer                       :: identifier_
        logical                       :: checking = .false., marked = .false., has_label_ = .false., defined_ = .false.
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
        procedure, public  :: get_label,      has_label
        procedure, public  :: get_attributes, has_attributes

        procedure, private :: read_formatted
        generic,   public  :: read(formatted) => read_formatted
        procedure, private :: write_formatted
        generic,   public  :: write(formatted) => write_formatted
    end type vertex_t

    interface vertex_t
      
      module function from_json(json_object) result(vertex)
        implicit none
        type(json_object_t), intent(in) :: json_object
        type(vertex_t) :: vertex
      end function
  
      pure module function construct_from_components(identifier, edges, label, attributes) result(vertex)
        implicit none
        integer, intent(in) :: identifier
        integer, intent(in) :: edges(:)
        type(varying_string), intent(in) :: label
        type(varying_string), intent(in), optional :: attributes
        type(vertex_t) vertex
      end function
      
    end interface

    interface

       module subroutine set_edge_vector(self,edges)
         !! Define the vertices on which this vertex depends on
         implicit none
         class(vertex_t),intent(inout)     :: self
         integer, intent(in) :: edges(:)
       end subroutine set_edge_vector

       impure elemental module function to_json(self) result(json_object)
         !! Result is a JSON representation of self
         implicit none
         class(vertex_t), intent(in) :: self
         type(json_object_t) :: json_object
       end function

       module subroutine add_edge(self,edge)
         implicit none
         class(vertex_t),intent(inout) :: self
         integer, intent(in) :: edge
       end subroutine add_edge

       module subroutine set_checking(self,checking)
         !! Set the status of the vertex in the toposort algorithm.  {True, False} => {being checked, not being checked},
         !! where being checked implies there is a circular graph.
         implicit none
         class(vertex_t), intent(inout) :: self
         logical, intent(in) :: checking
       end subroutine

       module subroutine set_marked(self,marked)
         !! Set the status of the vertex in the toposort algorithm.  {True, False} => {sort finished, not finished}.
         implicit none
         class(vertex_t), intent(inout) :: self
         logical, intent(in) :: marked
       end subroutine

       elemental module subroutine set_vertex_id(self,id)
         !! Set the vertex number -- not currently checked for uniqueness or order.
         implicit none
         class(vertex_t),intent(inout) :: self
         integer, intent(in) :: id
       end subroutine

       pure module function get_vertex_id(self) result(my_vertex_id)
         !! Get the vertex number associated with the vertex index.
         implicit none
         class(vertex_t), intent(in) :: self
         integer my_vertex_id
       end function

       pure module function get_edges(self) result(my_edges)
         !! Get the dependent vertices that this vertex depends on
         implicit none
         class(vertex_t), intent(in) :: self
         integer :: my_edges(size(self%edges))
       end function

       pure module function get_checking(self) result(my_checking)
         !! Get the status of the checking variable.
         implicit none
         class(vertex_t), intent(in) :: self
         logical my_checking
       end function

       pure module function get_marked(self) result(my_marked)
        !! Get the status of the marked variable.
         implicit none
         class(vertex_t), intent(in) :: self
         logical my_marked
       end function

       pure module function get_label(self) result(my_label)
         implicit none
         class(vertex_t), intent(in) :: self
         character(len=len(self%label)) my_label
       end function

       pure module function get_attributes(self) result(my_attributes)
         implicit none
         class(vertex_t), intent(in) :: self
         character(len=len(self%attributes)) my_attributes
       end function

       pure module function has_label(self) result(allocated_label)
         implicit none
         class(vertex_t), intent(in) :: self
         logical allocated_label
       end function

       pure module function has_attributes(self) result(allocated_attributes)
         implicit none
         class(vertex_t), intent(in) :: self
         logical allocated_attributes
       end function

       module subroutine read_formatted(self, unit, iotype, vlist, iostat, iomsg)
         !! Read a vertex from a JSON file
         implicit none
         class(vertex_t), intent(inout) :: self
         integer, intent(in) :: unit
         character (len=*), intent(in) :: iotype
         integer, intent(in) :: vlist(:)
         integer, intent(out) :: iostat
         character (len=*), intent(inout) :: iomsg
       end subroutine

       module subroutine write_formatted(self, unit, iotype, vlist, iostat, iomsg)
         !! Write a vertex to a JSON file using Everythingfunctional/jsonff
         implicit none
         class(vertex_t), intent(in) :: self
         integer, intent(in) :: unit
         character (len=*), intent(in) :: iotype
         integer, intent(in) :: vlist(:)
         integer, intent(out) :: iostat
         character (len=*), intent(inout) :: iomsg
       end subroutine

    end interface

end module vertex_m
