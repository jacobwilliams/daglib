module dag_interface
    use vertex_interface, only : vertex
    use jsonff, only : JsonObject_t

    implicit none

    private

    integer, parameter :: unset=0

    type,public :: dag
!! author: Jacob Williams, Damian Rouson, Robert Singleterry, Brad Richardson
!! version: v1.0
!! date: 2020-Nov-30
!! license: Copyright (c) 2020, Sourcery Institute, BSD 3-clause license Copyright (c) 2018 Jacob Williams
!!<hr />
!! DAG Module
!!<hr />
!! A directed acyclic graph (DAG) described at <https://en.wikipedia.org/wiki/Directed_acyclic_graph>
!! A graph that does not have any circular references or no path that can start at vertex v and end up at vertex v again.
!!
!! Private data type: vertex (from vertex_interface) and is an allocatable rank 1 array
!!
!! This data type represents the vertcies and edges of a DAG.
!!   - vertex: a task to be performed number as label
!!   - edge: incoming and outgoing dependences on that task to other task numbers
!!
!! A GCC bug exists from vertex_interface/vertex_implementation which effects these modules:
!!
!!   - dag_get_edges
!!   - dag_get_dependencies
!!   - dag_toposort/dfs
!!   - dag_generate_digraph
!!   - dag_generate_dependency_matrix
!!
!!<hr />
!! Public DAG Interfaces:
!!
!!    me%set_vertices ( Number of Vertices )
!!
!!       - Allocates the vertices variable and fills it with the array index number
!!
!!    me%set_edges ( Vertex Index, Edges for that Vertex )
!!
!!       - Allocates the edges for that vertex and fills them with the edges passed
!!
!!    me%set_vertex_info ( Vertex Index, Label (optional), Diagraph Attribute (optional) )
!!
!!       - Sets a default (Vertex Index) or input Label (me%vertices(index)%set_label)
!!       - Sets a diagraph attribute if input (me%vertices(index)%set_atrributes
!!
!!    me%toposort ( Sorted Vertex Order, Status )
!!
!!       - Rank 1 integer array of vertex numbers/labels in order of execution that adheres to dependencies
!!       - Status is 0 for no circular dependencies and 1 for circular dependencies
!!
!!    me%save_digraph ( File Name, Rank Direction, DPI )
!!
!!       - The file name of the saved digraph file
!!       - Rank Direction which are applicable inputs to the -rankdir option on the digraph command
!!       - DPI is the numerical dots per inch value
!!
!!    me%generate_dependency_matrix ( Dependencies )
!!
!!       - Output logical rank 2 array where .true. designates that rank 1 task depends on rank 2 task and .false. designates no dependence
!!
!!    edges = me%get_edges ( Vertex Index )
!!
!!       - Result: an allocatable rank 1 array of the vertices that this vertex (Vertex Index) depends on
!!
!!    deps = me%get_dependencies ( Vertix Index )
!!
!!       - Result: an allocatable rank 1 array of the vertices that depends on this vertex (Vertex Index)
!!
!!<hr />
!! Private DAG Interfaces:
!!
!!    str = me%dag_generate_digraph ( Rank Direction, DPI )
!!
!!       - Result is the string to write out to a file (.dot)
!!       - Rank Direction which are applicable inputs to the -rankdir option on the digraph command
!!       - DPI is the numerical dots per inch value
!!       - Called by dag_save_digraph()
!!
!!<hr />
!! Read and Write formatted input/output:
!!
!!    read_formatted ()
!!
!!       - Read a DAG from a JSON file using Everythingfunctional/jsonff
!!
!!    write_formatted ()
!!
!!       - Write a DAG to a JSON file using Everythingfunctional/jsonff
!!
!!<hr />
!! Call order to create a sorted list of tasks that satisfies the graph:
!!
!!    type ( dag ) :: d                ! set the DAG to the variable d
!!    integer :: n_nodes = 14          ! number of vertices
!!    integer, allocatable :: order(:) ! output for topo sort
!!
!!    call d%set_vertices(n_nodes)     ! set the number of nodes
!!
!!    call d%set_edges( 1,[2,3])       !  1 depends on  2 and 3
!!    call d%set_edges( 2,[integer::]) !  2 depends on nothing
!!    call d%set_edges( 3,[integer::]) !  3 depends on nothing
!!    call d%set_edges( 4,[3])         !  4 depends on  3
!!    call d%set_edges( 5,[4])         !  5 depends on  4
!!    call d%set_edges( 6,[3])         !  6 depends on  3
!!    call d%set_edges( 7,[4])         !  7 depends on  4
!!    call d%set_edges( 8,[5])         !  8 depends on  5
!!    call d%set_edges( 9,[6])         !  9 depends on  6
!!    call d%set_edges(10,[6,7])       ! 10 depends on  6 and 7
!!    call d%set_edges(11,[7,8])       ! 11 depends on  7 and 8
!!    call d%set_edges(12,[9])         ! 12 depends on  9
!!    call d%set_edges(13,[10])        ! 13 depends on 10
!!    call d%set_edges(14,[11])        ! 14 depends on 11
!!
!!    call d%toposort(order,istat)     ! perform the sort, istat = 0, no errors ; istat = 1, circular
!!
!!<hr />
!! Call order to create a digraph:
!!
!!    character(len=*), parameter :: gray_square = 'shape=square,fillcolor="SlateGray1",style=filled'
!!    character(len=len(gray_square)), parameter :: silk_circle = 'shape=circle,fillcolor="cornsilk",style=filled'
!!
!!    do i = 1, n_nodes
!!      call d%set_vertex_info(i, attributes = merge(gray_square, silk_circle, any(i==[1,2,12,13,14])))
!!    end do
!!
!!    call d%save_digraph('test.dot','RL',300)
!!
!! A digraph file (.dot) is written by dag_save_digraph and can be converted to a PDF using dot:
!!
!!    % dot -Tpdf -o test.pdf test.dot
!!
!!<hr />
!! Call order to generate the dependency matrix:
!!
!!    logical, allocatable :: mat(:,:)
!!
!!    call d%generate_dependency_matrix(mat)
!!
!!<hr />
!! Call order to get dependancy information for vertex V
!!
!!    integer :: v = 10
!!    integer,allocatable,dimension(:) :: edges, deps
!!
!!    edges = me%get_edges ( v )              ! result: [6,7]
!!    deps  = me%get_dependencies ( v )       ! result: [13]
!!
!!<hr />
!
        private
        type(vertex),dimension(:),allocatable :: vertices
    contains
        procedure,public  :: to_json
        procedure,private :: generate_digraph           => dag_generate_digraph

        procedure,public  :: set_vertices               => dag_set_vertices
        procedure,public  :: set_edges                  => dag_set_edges
        procedure,public  :: set_vertex_info            => dag_set_vertex_info
        procedure,public  :: toposort                   => dag_toposort
        procedure,public  :: generate_dependency_matrix => dag_generate_dependency_matrix
        procedure,public  :: save_digraph               => dag_save_digraph
        procedure,public  :: get_edges                  => dag_get_edges
        procedure,public  :: get_dependencies           => dag_get_dependencies

        procedure,private :: read_formatted
        generic,  public  :: read(formatted) => read_formatted
        procedure,private :: write_formatted
        generic,  public  :: write(formatted) => write_formatted
    end type dag

    interface dag
      procedure from_json
    end interface

    interface

!*******************************************************************************
       pure module function to_json(me) result(me_json)
         implicit none
         class(dag), intent(in) :: me
         type(JsonObject_t) :: me_json
       end function
!*******************************************************************************
       pure module function from_json(me_json) result(me)
         implicit none
         type(JsonObject_t), intent(in) :: me_json
         type(dag) :: me
       end function
!*******************************************************************************
       pure module function dag_get_edges(me,ivertex) result(edges)
         implicit none
         class(dag),intent(in)            :: me
         integer,intent(in)               :: ivertex
         integer,dimension(:),allocatable :: edges
       end function dag_get_edges
!*******************************************************************************
       pure module function dag_get_dependencies(me,ivertex) result(dep)
         implicit none
         class(dag),intent(in)            :: me
         integer,intent(in)               :: ivertex
         integer,dimension(:),allocatable :: dep
       end function dag_get_dependencies
!*******************************************************************************
       module subroutine dag_set_vertices(me,nvertices)
         implicit none
         class(dag),intent(inout)         :: me
         integer,intent(in)               :: nvertices
       end subroutine dag_set_vertices
!*******************************************************************************
       module subroutine dag_set_vertex_info(me,ivertex,label,attributes)
         implicit none
         class(dag),intent(inout)             :: me
         integer,intent(in)                   :: ivertex
         character(len=*),intent(in),optional :: label
         character(len=*),intent(in),optional :: attributes
       end subroutine dag_set_vertex_info
!*******************************************************************************
       module subroutine dag_set_edges(me,ivertex,edges)
         implicit none
         class(dag),intent(inout)        :: me
         integer,intent(in)              :: ivertex
         integer,dimension(:),intent(in) :: edges
       end subroutine dag_set_edges
!*******************************************************************************
       module subroutine dag_toposort(me,order,istat)
         implicit none
         class(dag),intent(inout)                     :: me
         integer,dimension(:),allocatable,intent(out) :: order
         integer,intent(out)                          :: istat
       end subroutine dag_toposort
!*******************************************************************************
       module function dag_generate_digraph(me,rankdir,dpi) result(str)
         implicit none
         class(dag),intent(in)                :: me
         character(len=:),allocatable         :: str
         character(len=*),intent(in),optional :: rankdir
         integer,intent(in),optional          :: dpi
       end function dag_generate_digraph
!*******************************************************************************
       module subroutine dag_generate_dependency_matrix(me,mat)
         implicit none
         class(dag),intent(in) :: me
         logical,dimension(:,:),intent(out),allocatable :: mat !! dependency matrix
       end subroutine dag_generate_dependency_matrix
!*******************************************************************************
       module subroutine dag_save_digraph(me,filename,rankdir,dpi)
         implicit none
         class(dag),intent(in) :: me
         character(len=*),intent(in),optional :: filename !! file name for diagraph
         character(len=*),intent(in),optional :: rankdir !! right to left orientation (e.g. 'RL')
         integer,intent(in),optional :: dpi !! resolution (e.g. 300)
       end subroutine dag_save_digraph
!*******************************************************************************
       module subroutine read_formatted(me, unit, iotype, vlist, iostat, iomsg)
         implicit none
         class(dag),intent(inout) :: me
         integer, intent(in) :: unit
         character (len=*), intent(in) :: iotype
         integer, intent(in) :: vlist(:)
         integer, intent(out) :: iostat
         character (len=*), intent(inout) :: iomsg
       end subroutine
!*******************************************************************************
       module subroutine write_formatted(me, unit, iotype, vlist, iostat, iomsg)
         class(dag), intent(in) :: me
         integer, intent(in) :: unit
         character (len=*), intent(in) :: iotype
         integer, intent(in) :: vlist(:)
         integer, intent(out) :: iostat
         character (len=*), intent(inout) :: iomsg
       end subroutine write_formatted
!*******************************************************************************

    end interface

end module dag_interface
