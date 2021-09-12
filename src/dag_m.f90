module dag_m
  !! summary: A directed acyclic graph (DAG) abstraction.
  !! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
  !! version: v1.0
  !! date: 2020-Nov-30
  !! license: Copyright (c) 2020, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
  use vertex_m, only : vertex_t
  use jsonff, only : json_object_t
  use iso_varying_string, only : varying_string

  implicit none

  private

  type,public :: dag_t
    !! Encapsulate a graph as an array of vertices, each storing dependency information
    private
    type(vertex_t),dimension(:),allocatable :: vertices
    integer, allocatable :: order(:)
  contains
    procedure :: is_sorted
    procedure :: to_json
    procedure :: save_digraph
    procedure :: dependency_matrix
    procedure :: num_vertices
    procedure :: dependencies_for
    generic :: write(formatted) => write_formatted
    generic :: read(formatted) => read_formatted

    procedure, private :: write_formatted
    procedure, private :: read_formatted

  end type

  interface dag_t

    module function construct_from_json(json_object) result(dag)
      implicit none
      type(json_object_t), intent(in) :: json_object
      type(dag_t) dag
    end function

    module function construct_from_components(vertices) result(dag)
      !! Construct an object containing the topologically sorted vertex ordering for a DAG
      implicit none
      type(vertex_t), intent(in) :: vertices(:)
      type(dag_t) dag
    end function

  end interface

  interface

    module function is_sorted(self)
      !! Result is true if dag%order contains a topological sorting of vertex identifiers
      implicit none
      class(dag_t), intent(in) :: self
      logical is_sorted
    end function

    module function to_json(self) result(json_object)
      implicit none
      class(dag_t), intent(in) :: self
      type(json_object_t) json_object
    end function

    module subroutine dependency_matrix(self,mat)
      !! Output array in which .true. elements are located at locations corresponding to dependencies
      implicit none
      class(dag_t),intent(in) :: self
      logical,dimension(:,:),intent(out),allocatable :: mat !! dependency matrix
    end subroutine

    pure module function num_vertices(self)
      !! Result is the size of the vertex array
      implicit none
      class(dag_t), intent(in) :: self
      integer num_vertices
    end function

    pure module function dependencies_for(self, vertex_id) result(dependency_ids)
      !! Result is an array of the ids on which vertex_id depends
      implicit none
      class(dag_t), intent(in) :: self
      integer, intent(in) :: vertex_id
      integer, allocatable :: dependency_ids(:)
    end function

    module subroutine save_digraph(self,filename,rankdir,dpi)
      implicit none
      class(dag_t),intent(in) :: self
      character(len=*),intent(in),optional :: filename !! digraph output file name
      character(len=*),intent(in),optional :: rankdir !! rank direction with right-to-left orientation (e.g. 'RL')
      integer,intent(in),optional :: dpi !! resolution in dots per inch (e.g. 300)
    end subroutine

    module subroutine read_formatted(self, unit, iotype, vlist, iostat, iomsg)
      !! Read a DAG from a JSON file using Everythingfunctional/jsonff
      implicit none
      class(dag_t),intent(inout) :: self
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
    end subroutine

    module subroutine write_formatted(self, unit, iotype, vlist, iostat, iomsg)
      !! Write a DAG to a JSON file using Everythingfunctional/jsonff
      class(dag_t), intent(in) :: self
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
    end subroutine write_formatted

  end interface

end module dag_m
