!*******************************************************************************
!  DAG Module.

    module dag_interface

    use vertex_interface, only : vertex
    implicit none

    private

    integer, parameter :: unset=0

    type,public :: dag
        !! A directed acyclic graph (DAG)
        !!
        !! Private type: vertex (vertex_interface) allocatable rank 1
        !!
        !! This data type represents the vertcies and edges of a DAG.
        !!   vertex: a task to be performed number as label
        !!   edge: incoming and outgoing dependences on that task to other task numbers
        !!
        !! Private DAG Interfaces:
        !!
        !!    None
        !!
        !! Public DAG Interfaces:
        !!
        !!    me%set_vertices ( Number of Vertices )
        !!       Allocates the vertices variable and fills it with the array index number
        !!
        !!    me%set_edges ( Vertex Index, Edges for that Vertex )
        !!       Allocates the edges for that vertex and fills them with the edges passed
        !!
        !!    me%set_vertex_info ( Vertex Index, Label (optional), Diagraph Attribute (optional) )
        !!       Sets a default (Vertex Index) or input Label (me%vertices(index)%set_label)
        !!       Sets a diagraph attribute if input (me%vertices(index)%set_atrributes
        !!
        !!   me%toposort ( Sorted Vertex Order, Status )
        !!       Rank 1 integer array of vertex numbers/labels in order of execution that adheres to dependencies
        !!       Status is 0 for no circular dependencies and 1 for circular dependencies
        !!
        !!   me%generate_digraph ( Rank Direction, DPI )
        !!       Rank Direction which are applicable inputs to the -rankdir option on the digraph command
        !!       DPI is the numerical dots per inch value
        !!
        !!   me%generate_dependency_matrix ( Dependencies )
        !!       Output logical rank 2 array where .true. designates that rank 1 task depends on rank 2 task and .false. designates no dependence
        !!
        !!   me%save_digraph ( File Name, Rank Direction, DPI )
        !!       The file name of the saved digraph file
        !!       Rank Direction which are applicable inputs to the -rankdir option on the digraph command
        !!       DPI is the numerical dots per inch value
        !!
        !!   me%get_edges ( 
        !!
        private
        type(vertex),dimension(:),allocatable :: vertices  !! the vertices in the DAG.
    contains
        procedure,public :: set_vertices     => dag_set_vertices
        procedure,public :: set_edges        => dag_set_edges
        procedure,public :: set_vertex_info  => dag_set_vertex_info
        procedure,public :: toposort         => dag_toposort
        procedure,public :: generate_digraph => dag_generate_digraph
        procedure,public :: generate_dependency_matrix => dag_generate_dependency_matrix
        procedure,public :: save_digraph     => dag_save_digraph
        procedure,public :: get_edges        => dag_get_edges
        procedure,public :: get_dependencies => dag_get_dependencies
        procedure,public :: read_formatted
        generic :: read(formatted) => read_formatted
        procedure,public :: write_formatted
        generic :: write(formatted) => write_formatted
    end type dag

interface

!*******************************************************************************
!  get the edges for the vertex (all the the vertices
!  that this vertex depends on).

    pure module function dag_get_edges(me,ivertex) result(edges)
    implicit none
    class(dag),intent(in) :: me
    integer,intent(in) :: ivertex
    integer,dimension(:),allocatable :: edges
    end function dag_get_edges
!*******************************************************************************

!*******************************************************************************
!  get all the vertices that depend on this vertex.

    pure module function dag_get_dependencies(me,ivertex) result(dep)
    implicit none
    class(dag),intent(in) :: me
    integer,intent(in) :: ivertex
    integer,dimension(:),allocatable :: dep  !! the set of all vertices
    end function dag_get_dependencies
!*******************************************************************************

!*******************************************************************************
!  set the number of vertices in the dag

    module subroutine dag_set_vertices(me,nvertices)
    implicit none
    class(dag),intent(inout) :: me
    integer,intent(in)       :: nvertices !! number of vertices
    end subroutine dag_set_vertices
!*******************************************************************************

!*******************************************************************************
!  set info about a vertex in a dag.

    module subroutine dag_set_vertex_info(me,ivertex,label,attributes)
    implicit none
    class(dag),intent(inout) :: me
    integer,intent(in)                   :: ivertex !! vertex number
    character(len=*),intent(in),optional :: label !! if a label is not set, then the integer vertex number is used.
    character(len=*),intent(in),optional :: attributes !! other attributes when
                                                       !! saving as a diagraph.
    end subroutine dag_set_vertex_info
!*******************************************************************************

!*******************************************************************************
!  set the edges for a vertex in a dag

    module subroutine dag_set_edges(me,ivertex,edges)
    implicit none
    class(dag),intent(inout)        :: me
    integer,intent(in)              :: ivertex !! vertex number
    integer,dimension(:),intent(in) :: edges
    end subroutine dag_set_edges
!*******************************************************************************

!*******************************************************************************
!  Main toposort routine

    module subroutine dag_toposort(me,order,istat)
    implicit none
    class(dag),intent(inout) :: me
    integer,dimension(:),allocatable,intent(out) :: order  !! the output toposort order
    integer,intent(out) :: istat !! Status flag:
                                 !!
                                 !! * 0 if no errors
                                 !! * -1 if circular dependency
                                 !!  (in this case, `order` will not be allocated)
    end subroutine dag_toposort
!*******************************************************************************

!*******************************************************************************
!  Generate a Graphviz digraph structure for the DAG.
!
!### Example
!  * To convert this to a PDF using `dot`: `dot -Tpdf -o test.pdf test.dot`,
!    where `test.dot` is `str` written to a file.

    module function dag_generate_digraph(me,rankdir,dpi) result(str)
    implicit none
    class(dag),intent(in) :: me
    character(len=:),allocatable :: str
    character(len=*),intent(in),optional :: rankdir !! right to left orientation (e.g. 'RL')
    integer,intent(in),optional :: dpi !! resolution (e.g. 300)
    end function dag_generate_digraph
!*******************************************************************************

!*******************************************************************************
!  Generate the dependency matrix for the DAG.
!
!  This is an \(n \times n \) matrix with elements \(A_{ij}\),
!  such that \(A_{ij}\) is true if vertex \(i\) depends on vertex \(j\).

    module subroutine dag_generate_dependency_matrix(me,mat)
    implicit none
    class(dag),intent(in) :: me
    logical,dimension(:,:),intent(out),allocatable :: mat !! dependency matrix
    end subroutine dag_generate_dependency_matrix
!*******************************************************************************

!*******************************************************************************
!  Generate a Graphviz digraph structure for the DAG and write it to a file.

    module subroutine dag_save_digraph(me,filename,rankdir,dpi)
    implicit none
    class(dag),intent(in) :: me
    character(len=*),intent(in),optional :: filename !! file name for diagraph
    character(len=*),intent(in),optional :: rankdir !! right to left orientation (e.g. 'RL')
    integer,intent(in),optional :: dpi !! resolution (e.g. 300)
    end subroutine dag_save_digraph
!*******************************************************************************

!*******************************************************************************
!>
!  Read the dag components from a file.

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

!*******************************************************************************
!>
!  Write the dag components to a file unit.

    module subroutine write_formatted(me, unit, iotype, vlist, iostat, iomsg)
    class(dag), intent(in) :: me
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    end subroutine
!*******************************************************************************

end interface

!*******************************************************************************
    end module dag_interface
!*******************************************************************************
