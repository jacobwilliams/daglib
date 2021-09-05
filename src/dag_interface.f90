module dag_interface
  !! summary: A directed acyclic graph (DAG) abstraction.
  !! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
  !! version: v1.0
  !! date: 2020-Nov-30
  !! license: Copyright (c) 2020, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
  use vertex_interface, only : vertex_t
  use jsonff, only : json_object_t
  use iso_varying_string, only : varying_string

  implicit none

  private

  type,public :: dag_t
    !! Encapsulate a graph as an array of vertices, each storing dependency information
      private
      type(vertex_t),dimension(:),allocatable :: vertices
    contains
      procedure,public  :: to_json
      procedure,private :: generate_digraph           => dag_generate_digraph
      procedure         :: set_vertex_label
      procedure         :: set_vertex_attributes
      procedure,public  :: set_vertices               => dag_set_vertices
      procedure,public  :: set_edges                  => dag_set_edges
      procedure,public  :: toposort                   => dag_toposort
      procedure,public  :: generate_dependency_matrix => dag_generate_dependency_matrix
      procedure,public  :: save_digraph               => dag_save_digraph
      procedure,public  :: get_num_vertices           => dag_get_num_vertices
      procedure,public  :: get_edges                  => dag_get_edges
      procedure,public  :: get_dependencies           => dag_get_dependencies

      procedure,private :: read_formatted
      generic,  public  :: read(formatted) => read_formatted
      procedure,private :: write_formatted
      generic,  public  :: write(formatted) => write_formatted
    end type dag_t

    interface dag_t
      procedure from_json
    end interface

    interface

      module function to_json(me) result(me_json)
        implicit none
        class(dag_t), intent(in) :: me
        type(json_object_t) :: me_json
      end function

      module function from_json(me_json) result(me)
        implicit none
        type(json_object_t), intent(in) :: me_json
        type(dag_t) :: me
      end function

      pure module function dag_get_num_vertices(me) result(num_vertices)
        implicit none
        class(dag_t), intent(in) :: me
        integer num_vertices
      end function

      pure module function dag_get_edges(me,ivertex) result(edges)
        !! Result: array of the vertex numbers on which this vertex depends
        implicit none
        class(dag_t),intent(in)            :: me
        integer,intent(in)               :: ivertex
        integer,dimension(:),allocatable :: edges
      end function dag_get_edges

      pure module function dag_get_dependencies(me,ivertex) result(dep)
        !! Result: array of the vertices that depend on ivertex vertex
        implicit none
        class(dag_t),intent(in)            :: me
        integer,intent(in)               :: ivertex
        integer,dimension(:),allocatable :: dep
      end function dag_get_dependencies

      module subroutine dag_set_vertices(me,nvertices)
        !! Allocates the vertices variable and fills it with the array index number
        implicit none
        class(dag_t),intent(inout)         :: me
        integer,intent(in)               :: nvertices
      end subroutine dag_set_vertices

      module subroutine set_vertex_label(me, ivertex, label)
        implicit none
        class(dag_t), intent(inout)  :: me
        integer, intent(in)          :: ivertex(:)
        type(varying_string), intent(in), optional :: label(:)
      end subroutine

      module subroutine set_vertex_attributes(me, ivertex, attributes)
        implicit none
        class(dag_t), intent(inout)  :: me
        integer, intent(in)          :: ivertex
        character(len=*), intent(in) :: attributes
      end subroutine

      module subroutine dag_set_edges(me,ivertex,edges)
        !! Allocates the edges for that vertex and fills them with the edges passed
        implicit none
        class(dag_t),intent(inout)        :: me
        integer,intent(in)              :: ivertex 
        integer,dimension(:),intent(in) :: edges
      end subroutine dag_set_edges

      module subroutine dag_toposort(me,order,istat)
        !! Provide array of vertex numbers order in a way that respects dependencies
        implicit none
        class(dag_t),intent(inout)                   :: me
        integer,dimension(:),allocatable,intent(out) :: order !! sorted vertex order
        integer,intent(out)                          :: istat !! 0 for no circular dependencies, 1 for circular dependencies
      end subroutine dag_toposort

      module function dag_generate_digraph(me,rankdir,dpi) result(str)
        !! - Result is the string to write out to a *.dot file. (Called by dag_save_digraph())
        implicit none
        class(dag_t),intent(in)                :: me
        character(len=:),allocatable         :: str
        character(len=*),intent(in),optional :: rankdir
          !! - Rank Direction which are applicable inputs to the -rankdir option on the digraph command
        integer,intent(in),optional          :: dpi
          !! - dots per inch 
      end function dag_generate_digraph

      module subroutine dag_generate_dependency_matrix(me,mat)
        !! Output array in which .true. elements are located at locations corresponding to dependencies
        implicit none
        class(dag_t),intent(in) :: me
        logical,dimension(:,:),intent(out),allocatable :: mat !! dependency matrix
      end subroutine dag_generate_dependency_matrix

      module subroutine dag_save_digraph(me,filename,rankdir,dpi)
        implicit none
        class(dag_t),intent(in) :: me
        character(len=*),intent(in),optional :: filename !! digraph output file name
        character(len=*),intent(in),optional :: rankdir !! rank direction with right-to-left orientation (e.g. 'RL')
        integer,intent(in),optional :: dpi !! resolution in dots per inch (e.g. 300)
      end subroutine dag_save_digraph

      module subroutine read_formatted(me, unit, iotype, vlist, iostat, iomsg)
        !! Read a DAG from a JSON file using Everythingfunctional/jsonff
        implicit none
        class(dag_t),intent(inout) :: me
        integer, intent(in) :: unit
        character (len=*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character (len=*), intent(inout) :: iomsg
      end subroutine

      module subroutine write_formatted(me, unit, iotype, vlist, iostat, iomsg)
        !! Write a DAG to a JSON file using Everythingfunctional/jsonff
        class(dag_t), intent(in) :: me
        integer, intent(in) :: unit
        character (len=*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character (len=*), intent(inout) :: iomsg
      end subroutine write_formatted

    end interface

end module dag_interface
