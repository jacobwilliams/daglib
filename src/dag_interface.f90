!*******************************************************************************
!>
!  DAG Module.

    module dag_interface

    use vertex_interface, only : vertex
    implicit none

    private

    integer, parameter :: unset=0

    type,public :: dag
        !! a directed acyclic graph (DAG)
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
        procedure,public :: input
        procedure,public :: write_formatted
        generic :: write(formatted) => write_formatted
    end type dag

interface

!*******************************************************************************
!>
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
!>
!  get all the vertices that depend on this vertex.

    pure module function dag_get_dependencies(me,ivertex) result(dep)
    implicit none
    class(dag),intent(in) :: me
    integer,intent(in) :: ivertex
    integer,dimension(:),allocatable :: dep  !! the set of all vertices
    end function dag_get_dependencies
!*******************************************************************************

!*******************************************************************************
!>
!  set the number of vertices in the dag

    module subroutine dag_set_vertices(me,nvertices)
    implicit none
    class(dag),intent(inout) :: me
    integer,intent(in)       :: nvertices !! number of vertices
    end subroutine dag_set_vertices
!*******************************************************************************

!*******************************************************************************
!>
!  set info about a vertex in a dag.

    module subroutine dag_set_vertex_info(me,ivertex,label,attributes)
    implicit none
    class(dag),intent(inout) :: me
    integer,intent(in)                   :: ivertex !! vertex number
    character(len=*),intent(in),optional :: label !! if a label is not set,
                                                  !! then the integer vertex
                                                  !! number is used.
    character(len=*),intent(in),optional :: attributes !! other attributes when
                                                       !! saving as a diagraph.
    end subroutine dag_set_vertex_info
!*******************************************************************************

!*******************************************************************************
!>
!  set the edges for a vertex in a dag

    module subroutine dag_set_edges(me,ivertex,edges)
    implicit none
    class(dag),intent(inout)        :: me
    integer,intent(in)              :: ivertex !! vertex number
    integer,dimension(:),intent(in) :: edges
    end subroutine dag_set_edges
!*******************************************************************************

!*******************************************************************************
!>
!  Main toposort routine

    module subroutine dag_toposort(me,order,istat)
    implicit none
    class(dag),intent(inout) :: me
    integer,dimension(:),allocatable,intent(out) :: order  !! the toposort order
    integer,intent(out) :: istat !! Status flag:
                                 !!
                                 !! * 0 if no errors
                                 !! * -1 if circular dependency
                                 !!  (in this case, `order` will not be allocated)
    end subroutine dag_toposort
!*******************************************************************************

!*******************************************************************************
!>
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
!>
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
!>
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

    module subroutine input(me,filename)
    implicit none
    class(dag),intent(out) :: me
    character(len=*),intent(in) :: filename !! file name for diagraph
    end subroutine
!*******************************************************************************

!*******************************************************************************
!>
!  Write the dag components to a file unit.

    module subroutine write_formatted(this, unit, iotype, vlist, iostat, iomsg)
    class(dag), intent(in) :: this
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
