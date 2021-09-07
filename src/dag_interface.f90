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
    procedure :: to_json
    procedure :: save_digraph => dag_save_digraph
    generic :: write(formatted) => write_formatted
    generic :: read(formatted) => read_formatted

    procedure, private :: write_formatted
    procedure, private :: read_formatted

    procedure, private  :: toposort                   => dag_toposort
    procedure, private  :: generate_dependency_matrix => dag_generate_dependency_matrix
  end type

  interface dag_t

    module function from_json(me_json) result(me)
      implicit none
      type(json_object_t), intent(in) :: me_json
      type(dag_t) :: me
    end function

    module function construct(vertices) result(new_dag)
      implicit none
      type(vertex_t), intent(in) :: vertices(:)
      type(dag_t) new_dag
    end function

  end interface

  interface

    module function to_json(me) result(me_json)
      implicit none
      class(dag_t), intent(in) :: me
      type(json_object_t) :: me_json
    end function

    module subroutine dag_toposort(me,order,istat)
      !! Provide array of vertex numbers order in a way that respects dependencies
      implicit none
      class(dag_t),intent(inout)                   :: me
      integer,dimension(:),allocatable,intent(out) :: order !! sorted vertex order
      integer,intent(out)                          :: istat !! 0 for no circular dependencies, 1 for circular dependencies
    end subroutine dag_toposort

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
