!*******************************************************************************
!>
!  DAG Module.

    module dag_module

    use iso_fortran_env

    implicit none

    private

#ifdef INT8
    integer,parameter,public :: daglib_ip = int8     !! Integer working precision [1 byte]
#elif INT16
    integer,parameter,public :: daglib_ip = int16    !! Integer working precision [2 bytes]
#elif INT32
    integer,parameter,public :: daglib_ip = int32    !! Integer working precision [4 bytes]
#elif INT64
    integer,parameter,public :: daglib_ip = int64    !! Integer working precision [8 bytes]
#else
    integer,parameter,public :: daglib_ip = int32    !! Integer working precision if not specified [4 bytes]
#endif
    integer,parameter :: ip = daglib_ip  !! local copy of `daglib_ip` with a shorter name

    integer(ip),parameter :: MAX_INT_STR_LEN = 64 !! maximum length of an integer string

    type :: edge
        !! the "to" vertex that defines an edge. This is part of
        !! the array of vertices contained without the "from" [[vertex]] type.
        !! an edge can also have optional attrubutes for graphviz.
        integer(ip) :: ivertex = 0 !! vertex number (the index in the [[dag]] `vertices` array)
        character(len=:),allocatable :: label !! used for diagraph
        character(len=:),allocatable :: attributes !! used for diagraph
        class(*),allocatable :: metadata !! user-defined metadata
    end type edge
    interface edge
        !! constructor for an [[edge]] type.
        procedure :: edge_constructor
    end interface edge

    type :: vertex
        !! a vertex (or node) of a directed acyclic graph (DAG)
        private
        type(edge),dimension(:),allocatable :: edges  !! these are the vertices that this vertex
                                                      !! depends on. (edges of the graph).
        integer(ip) :: ivertex = 0 !! vertex number (the index in the [[dag]] `vertices` array)
        logical :: checking = .false.  !! used for toposort
        logical :: marked = .false.    !! used for toposort
        character(len=:),allocatable :: label      !! used for diagraph
        character(len=:),allocatable :: attributes !! used for diagraph
        class(*),allocatable :: metadata !! user-defined metadata
    contains
        private
        generic :: set_edges => set_edge_vector_vector, add_edge
        procedure :: set_edge_vector_vector, add_edge
        procedure :: remove_edge
    end type vertex

    type,public :: dag
        !! a directed acyclic graph (DAG).
        !! a collection of vertices (nodes) that are connected to other vertices.
        private
        integer(ip) :: n = 0 !! number of vertices (size of `vertices` array)
        type(vertex),dimension(:),allocatable :: vertices  !! the vertices in the DAG. The index in
                                                           !! this array if the vertex number.
    contains
        private

        procedure,public :: vertex => dag_get_vertex !! not very useful for now, since
                                                     !! all vertex attributes are private
        procedure,public :: number_of_vertices  => dag_get_number_of_vertices
        procedure,public :: get_edge_metadata   => dag_get_edge_metadata
        procedure,public :: get_vertex_metadata => dag_get_vertex_metadata
        procedure,public :: get_edges           => dag_get_edges
        procedure,public :: get_dependencies    => dag_get_dependencies

        procedure,public :: set_vertices        => dag_set_vertices
        procedure,public :: set_vertex_info     => dag_set_vertex_info
        procedure,public :: add_edge            => dag_add_edge
        generic,public   :: set_edges           => dag_set_edges_no_atts, &
                                                   dag_set_edges_vector_atts
        procedure,public :: remove_edge         => dag_remove_edge
        procedure,public :: remove_vertex       => dag_remove_node
        procedure,public :: toposort            => dag_toposort
        procedure,public :: traverse            => dag_traverse
        procedure,public :: generate_digraph    => dag_generate_digraph
        procedure,public :: generate_dependency_matrix => dag_generate_dependency_matrix
        procedure,public :: save_digraph        => dag_save_digraph
        procedure,public :: destroy             => dag_destroy
        procedure,public :: get_edge_index

        procedure :: init_internal_vars !! private routine to initialize some internal variables
        procedure :: dag_set_edges_no_atts, dag_set_edges_vector_atts

    end type dag

    abstract interface
        subroutine traverse_func(ivertex,stop,iedge)
            !! user-provided function for traversing a dag.
            import :: ip
            implicit none
            integer(ip),intent(in) :: ivertex !! vertex number
            logical,intent(out) :: stop !! set to true to stop the process
            integer(ip),intent(in),optional :: iedge !! edge index for this vertex
                                                     !! (note: not the vertex number,
                                                     !! the index in the array of edge vertices)
                                                     !! [not present if this is the starting node]
        end subroutine traverse_func
    end interface

    contains
!*******************************************************************************

!*******************************************************************************
!>
!  Constructor for [[edge]] type.

    pure elemental function edge_constructor(ivertex,label,attributes,metadata) result(e)

    integer(ip),intent(in),optional :: ivertex !! vertex number defining the destination of this edge
    character(len=*),intent(in),optional :: label !! vertex name for grahviz
    character(len=*),intent(in),optional :: attributes !! other attributes for graphviz
    class(*),intent(in),optional :: metadata !! optional user-defined metadata

    type(edge) :: e
    e%ivertex = ivertex
    if (present(label)) e%label = label
    if (present(attributes)) e%attributes = attributes
    if (present(metadata)) allocate(e%attributes, source = attributes)

    end function edge_constructor
!*******************************************************************************

!*******************************************************************************
!>
!  Destroy the `dag`.

    subroutine dag_destroy(me)

    class(dag),intent(inout) :: me

    me%n = 0
    if (allocated(me%vertices)) deallocate(me%vertices)

    end subroutine dag_destroy
!*******************************************************************************

!*******************************************************************************
!>
!  specify the edge indices for this vertex

    subroutine set_edge_vector_vector(me,edges,label,attributes,metadata)

    class(vertex),intent(inout) :: me
    integer(ip),dimension(:),intent(in) :: edges
    character(len=*),dimension(:),intent(in),optional :: label
    character(len=*),dimension(:),intent(in),optional :: attributes !! other attributes when
                                                                    !! saving as a diagraph.
    class(*),intent(in),optional :: metadata !! optional user-defined metadata

    ! elemental assignment:
    me%edges = edge(ivertex=edges,label=label,&
                    attributes=attributes,metadata=metadata)
    call sort_ascending(me%edges)

    end subroutine set_edge_vector_vector
!*******************************************************************************

!*******************************************************************************
!>
!  add an edge index for this vertex

    subroutine add_edge(me,e,label,attributes,metadata)

    class(vertex),intent(inout) :: me
    integer(ip),intent(in) :: e
    character(len=*),intent(in),optional :: label
    character(len=*),intent(in),optional :: attributes !! other attributes when
                                                       !! saving as a diagraph.
    class(*),intent(in),optional :: metadata !! optional user-defined metadata

    type(edge) :: edge_

    edge_ = edge(ivertex=e,label=label,&
                 attributes=attributes,metadata=metadata)

    if (allocated(me%edges)) then
        if (.not. any(e==me%edges%ivertex)) then ! don't add if already there
            me%edges = [me%edges, edge_]
            call sort_ascending(me%edges)
        end if
    else
        me%edges = [edge_]
    end if

    end subroutine add_edge
!*******************************************************************************

!*******************************************************************************
!>
!  remove an edge index from this vertex

    subroutine remove_edge(me,e)

    class(vertex),intent(inout) :: me
    integer(ip),intent(in) :: e

    integer(ip),dimension(1) :: idx
    type(edge),dimension(:),allocatable :: tmp

    if (allocated(me%edges)) then
        idx = findloc(me%edges%ivertex, e)
        if (idx(1)>0) then
            ! the edge is in the list
            associate (i => idx(1), n => size(me%edges))
                if (n==1) then
                    deallocate(me%edges) ! it's the only one there
                else
                    allocate(tmp(n-1))
                    if (i>1) tmp(1:i-1) = me%edges(1:i-1)
                    if (i<n) tmp(i:n-1) = me%edges(i+1:n)
                    call move_alloc(tmp,me%edges)
                end if
            end associate
        end if
    end if

    end subroutine remove_edge
!*******************************************************************************

!*******************************************************************************
!>
!  Remove a node from a dag. Will also remove any edges connected to it.
!
!  This will renumber the nodes and edges internally.
!  Note that any default integer labels generated in
!  [[dag_set_vertices]] would then be questionable.

    subroutine dag_remove_node(me,ivertex)

    class(dag),intent(inout) :: me
    integer(ip),intent(in) :: ivertex !! the node to remove

    integer(ip) :: i !! counter
    type(vertex),dimension(:),allocatable :: tmp !! for resizing `me%vertices`

    if (allocated(me%vertices)) then
        associate (n => size(me%vertices))
            do i = 1, n
                ! first remove any edges:
                call me%vertices(i)%remove_edge(ivertex)
                ! next, renumber the existing edges so they will be
                ! correct after ivertex is deleted
                ! Example (removing 2): 1 [2] 3 4 ==> 1 2 3
                if (allocated(me%vertices(i)%edges)) then
                    where (me%vertices(i)%edges%ivertex>ivertex)
                        me%vertices(i)%edges%ivertex = me%vertices(i)%edges%ivertex - 1
                    end where
                end if
            end do
            ! now, remove the node:
            allocate(tmp(n-1))
            if (ivertex>1) tmp(1:ivertex-1) = me%vertices(1:ivertex-1)
            if (ivertex<n) tmp(ivertex:n-1) = me%vertices(ivertex+1:n)
            call move_alloc(tmp,me%vertices)
        end associate
    end if
    me%n = size(me%vertices)
    if (me%n==0) deallocate(me%vertices)

    end subroutine dag_remove_node
!*******************************************************************************

!*******************************************************************************
!>
!  get the edges for the vertex (all of the vertices
!  that this vertex depends on).

    pure function dag_get_edges(me,ivertex) result(edges)

    class(dag),intent(in) :: me
    integer(ip),intent(in) :: ivertex
    integer(ip),dimension(:),allocatable :: edges

    if (allocated(me%vertices(ivertex)%edges)) then
        if (ivertex>0 .and. ivertex <= me%n) then
            edges = me%vertices(ivertex)%edges%ivertex  ! auto LHS allocation
        end if
    end if

    end function dag_get_edges
!*******************************************************************************

!*******************************************************************************
!>
!  get all the vertices that depend on this vertex.

    pure function dag_get_dependencies(me,ivertex) result(dep)

    class(dag),intent(in) :: me
    integer(ip),intent(in) :: ivertex
    integer(ip),dimension(:),allocatable :: dep  !! the set of all vertices
                                             !! than depend on `ivertex`

    integer(ip) :: i !! vertex counter

    if (ivertex>0 .and. ivertex <= me%n) then

        ! have to check all the vertices:
        do i=1, me%n
            if (allocated(me%vertices(i)%edges)) then
                if (any(me%vertices(i)%edges%ivertex == ivertex)) then
                    if (allocated(dep)) then
                        dep = [dep, i]  ! auto LHS allocation
                    else
                        dep = [i]       ! auto LHS allocation
                    end if
                end if
            end if
        end do

    end if

    end function dag_get_dependencies
!*******************************************************************************

!*******************************************************************************
!>
!  set the number of vertices (nodes) in the dag.
!
!### See also
!  * [[dag_remove_node]] which can be used to remove a vertex.
!  * [[dag_set_vertex_info]] which can be used to set/change
!    the labels and other attributes.

    subroutine dag_set_vertices(me,nvertices,labels,attributes,metadata)

    class(dag),intent(inout) :: me
    integer(ip),intent(in)   :: nvertices !! number of vertices
    character(len=*),dimension(nvertices),intent(in),optional :: labels !! vertex name strings
    character(len=*),intent(in),optional :: attributes !! other attributes when
                                                       !! saving as a diagraph.
    class(*),intent(in),optional :: metadata !! optional user-defined metadata

    integer(ip) :: i !! counter
    logical :: has_label !! if `labels` is specified
    character(len=:),allocatable :: label_ !! temp variable for labels

    if (nvertices<=0) error stop 'error: nvertices must be >= 1'

    if (allocated(me%vertices)) deallocate(me%vertices)

    me%n = nvertices
    allocate(me%vertices(nvertices))
    me%vertices%ivertex = [(i,i=1,nvertices)] ! vertex indices

    has_label = present(labels)

    do i = 1, nvertices
        if (has_label) then
            label_ = trim(adjustl(labels(i)))
        else
            label_ = integer_to_string(i)  ! just use the vertex number
        end if
        call me%set_vertex_info(ivertex=i,label=label_,&
                                attributes=attributes,metadata=metadata)
    end do

    end subroutine dag_set_vertices
!*******************************************************************************

!*******************************************************************************
!>
!  Returns the number of vertices (nodes) in the dag.

    pure function dag_get_number_of_vertices(me) result(nvertices)

    class(dag),intent(in) :: me
    integer(ip) :: nvertices !! number of vertices

    nvertices = me%n

    end function dag_get_number_of_vertices
!*******************************************************************************

!*******************************************************************************
!>
!  Returns the metadata for a vertex (node) in the dag.

    pure function dag_get_vertex_metadata(me,ivertex) result(m)

    class(dag),intent(in) :: me
    integer(ip),intent(in) :: ivertex !! vertex number
    class(*),allocatable :: m

    if (allocated(me%vertices(ivertex)%metadata)) &
        allocate(m, source = me%vertices(ivertex)%metadata)

    end function dag_get_vertex_metadata
!*******************************************************************************

!*******************************************************************************
!>
!  Returns the metadata for an edge in the dag.

    pure function dag_get_edge_metadata(me,ivertex,iedge) result(m)

    class(dag),intent(in) :: me
    integer(ip),intent(in) :: ivertex !! vertex number
    integer(ip),intent(in) :: iedge   !! edge vertex
    class(*),allocatable   :: m

    associate ( i => me%get_edge_index(ivertex,iedge) )
        if (i>0) allocate(m, source = me%vertices(ivertex)%edges(i)%metadata)
    end associate

    end function dag_get_edge_metadata
!*******************************************************************************

!*******************************************************************************
!>
!  Returns the index in the edge array of the vertex.

    pure function get_edge_index(me,ivertex,iedge) result(edge_index)

    class(dag),intent(in) :: me
    integer(ip),intent(in) :: ivertex !! vertex number
    integer(ip),intent(in) :: iedge !! edge vertex number
    integer(ip) :: edge_index !! the index of the `iedge` vertex in
                              !! the edge array (0 if not found)

    integer(ip),dimension(1) :: idx

    if (allocated(me%vertices(ivertex)%edges)) then
        idx = findloc(me%vertices(ivertex)%edges%ivertex, iedge)
        edge_index = idx(1)
    else
        edge_index = 0_ip
    end if

    end function get_edge_index
!*******************************************************************************

!*******************************************************************************
!>
!  set info about a vertex in a dag.

    subroutine dag_set_vertex_info(me,ivertex,label,attributes,metadata)

    class(dag),intent(inout) :: me
    integer(ip),intent(in)               :: ivertex !! vertex number
    character(len=*),intent(in),optional :: label !! if a label is not set,
                                                  !! then the integer vertex
                                                  !! number is used.
    character(len=*),intent(in),optional :: attributes !! other attributes when
                                                       !! saving as a diagraph.
    class(*),intent(in),optional :: metadata !! optional user-defined metadata

    if (present(label)) me%vertices(ivertex)%label = label
    if (present(attributes)) me%vertices(ivertex)%attributes = attributes
    if (present(metadata)) allocate(me%vertices(ivertex)%metadata, source=metadata)

    end subroutine dag_set_vertex_info
!*******************************************************************************

!*******************************************************************************
!>
!  Get the `i`th vertex.
!
!  The program will stop if vertex `i` does not exist.

    function dag_get_vertex(me,i) result(v)

    class(dag),intent(inout) :: me
    integer(ip),intent(in) :: i !! vertex number
    type(vertex) :: v

    if (i<0 .or. i>me%n) then
        error stop 'Error in dag_get_vertex: invalid vertex number'
    else
        v = me%vertices(i)
    end if

    end function dag_get_vertex
!*******************************************************************************

!*******************************************************************************
!>
!  Add an edge to a dag.

    subroutine dag_add_edge(me,ivertex,iedge,label,attributes,metadata)

    class(dag),intent(inout) :: me
    integer(ip),intent(in)   :: ivertex !! vertex number
    integer(ip),intent(in)   :: iedge   !! the vertex to connect to `ivertex`
    character(len=*),intent(in),optional :: label !! edge label
    character(len=*),intent(in),optional :: attributes !! other attributes when
                                                       !! saving as a diagraph.
    class(*),intent(in),optional :: metadata !! optional user-defined metadata

    call me%vertices(ivertex)%set_edges(iedge,&
                        label=label,&
                        attributes=attributes,&
                        metadata=metadata)

    end subroutine dag_add_edge
!*******************************************************************************

!*******************************************************************************
!>
!  set the edges for a vertex in a dag

    subroutine dag_set_edges_no_atts(me,ivertex,edges)

    class(dag),intent(inout)            :: me
    integer(ip),intent(in)              :: ivertex !! vertex number
    integer(ip),dimension(:),intent(in) :: edges

    call me%vertices(ivertex)%set_edges(edges)

    end subroutine dag_set_edges_no_atts
!*******************************************************************************

!*******************************************************************************
!>
!  Remove an edge from a dag.

    subroutine dag_remove_edge(me,ivertex,iedge)

    class(dag),intent(inout) :: me
    integer(ip),intent(in)   :: ivertex !! vertex number
    integer(ip),intent(in)   :: iedge   !! the edge to remove

    call me%vertices(ivertex)%remove_edge(iedge)

    end subroutine dag_remove_edge
!*******************************************************************************

!*******************************************************************************
!>
!  set the edges for a vertex in a dag

    subroutine dag_set_edges_vector_atts(me,ivertex,edges,attributes,label)

    class(dag),intent(inout)            :: me
    integer(ip),intent(in)              :: ivertex !! vertex number
    integer(ip),dimension(:),intent(in) :: edges
    character(len=*),dimension(:),intent(in) :: attributes !! other attributes when
                                                           !! saving as a diagraph.
    character(len=*),dimension(:),intent(in),optional :: label

    call me%vertices(ivertex)%set_edges(edges,label=label,attributes=attributes)

    end subroutine dag_set_edges_vector_atts
!*******************************************************************************

!*******************************************************************************
!>
!  Initialize the internal private variables used for graph traversal.

    subroutine init_internal_vars(me)

    class(dag),intent(inout) :: me

    integer(ip) :: i !! counter

    do i = 1, me%n
        me%vertices(i)%marked = .false.
        me%vertices(i)%checking = .false.
    end do

    end subroutine init_internal_vars
!*******************************************************************************

!*******************************************************************************
!>
!  Main toposort routine

    subroutine dag_toposort(me,order,istat)

    class(dag),intent(inout) :: me
    integer(ip),dimension(:),allocatable,intent(out) :: order  !! the toposort order
    integer(ip),intent(out) :: istat !! Status flag:
                                     !!
                                     !! * 0 if no errors
                                     !! * -1 if circular dependency
                                     !!  (in this case, `order` will not be allocated)

    integer(ip) :: i,iorder

    if (me%n==0) return

    ! initialize internal variables, in case
    ! we have called this routine before.
    call me%init_internal_vars()

    allocate(order(me%n))
    iorder = 0  ! index in order array
    istat = 0   ! no errors so far
    do i=1,me%n
      if (.not. me%vertices(i)%marked) call dfs(me%vertices(i))
      if (istat==-1) exit
    end do

    if (istat==-1) deallocate(order)

    contains

    recursive subroutine dfs(v)

    !! depth-first graph traversal

    type(vertex),intent(inout) :: v
    integer(ip) :: j

    if (istat==-1) return

    if (v%checking) then
      ! error: circular dependency
      istat = -1
    else
      if (.not. v%marked) then
        v%checking = .true.
        if (allocated(v%edges)) then
          do j=1,size(v%edges)
            call dfs(me%vertices(v%edges(j)%ivertex))
            if (istat==-1) return
          end do
        end if
        v%checking = .false.
        v%marked = .true.
        iorder = iorder + 1
        order(iorder) = v%ivertex
      end if
    end if

    end subroutine dfs

    end subroutine dag_toposort
!*******************************************************************************

!*******************************************************************************
!>
!  depth-first graph traversal of the dag.
!
!  This will visit each node in the graph once, and call the `userfunc`.
!  If some nodes are not connected to `ivertex`, then they will not be visited.
!
!@todo Should also add a bfs option.

    subroutine dag_traverse(me,ivertex,userfunc)

    class(dag),intent(inout) :: me
    integer(ip),intent(in) :: ivertex !! the vertex number to start on
    procedure(traverse_func) :: userfunc !! a user-provided function that will
                                         !! be called for each vertex/edge combination

    if (me%n==0) return ! nothing to do
    if (ivertex<0 .or. ivertex>me%n) error stop 'invalid vertex number in dag_traverse'

    ! initialize internal variables, in case
    ! we have called this routine before.
    call me%init_internal_vars()

    call dfs(ivertex)

    contains

        recursive subroutine dfs(ivertex,iedge)
        !! depth-first graph traversal
        integer(ip),intent(in) :: ivertex !! the vertex
        integer(ip),intent(in),optional :: iedge !! the edge index for this vertex
        if (present(iedge)) then ! visiting an edge
            associate ( v => me%vertices(me%vertices(ivertex)%edges(iedge)%ivertex) )
                if (done(v,ivertex,iedge)) return
            end associate
        else ! the starting node, no edge
            associate ( v => me%vertices(ivertex) )
                if (done(v,ivertex,iedge)) return
            end associate
        end if
        end subroutine dfs

        recursive function done(v,iv,ie) result(user_stop)
            !! process this vertex in the [[dfs]] and return true if done.
            type(vertex),intent(inout) :: v !! vertex to process
            logical :: user_stop !! if the user has signaled to stop
            integer(ip),intent(in) :: iv !! the vertex number
            integer(ip),intent(in),optional :: ie !! the edge index for this vertex (if this is an edge)
            integer(ip) :: jedge !! edge counter
            if (v%marked) return ! this one has already been visited
            v%marked = .true.    !
            ! call the user's function for this node/edge combo:
            call userfunc(iv,user_stop,ie)
            if (.not. user_stop) then ! continue traversing
                if (allocated(v%edges)) then
                    do jedge = 1,size(v%edges)
                        call dfs(v%ivertex,jedge)
                        if (user_stop) return
                    end do
                end if
            end if
        end function done

    end subroutine dag_traverse
!*******************************************************************************

!*******************************************************************************
!>
!  Generate a Graphviz digraph structure for the DAG.
!
!### Example
!  * To convert this to a PDF using `dot`: `dot -Tpdf -o test.pdf test.dot`,
!    where `test.dot` is `str` written to a file.

    function dag_generate_digraph(me,rankdir,dpi) result(str)

    class(dag),intent(in) :: me
    character(len=:),allocatable :: str
    character(len=*),intent(in),optional :: rankdir !! right to left orientation (e.g. 'RL')
    integer(ip),intent(in),optional :: dpi !! resolution (e.g. 300)

    integer(ip) :: i,j     !! counter
    integer(ip) :: n_edges !! number of edges
    character(len=:),allocatable :: attributes !! full attributes string for node or edge
    logical :: compress !! if we can write all the edges on one line

    character(len=*),parameter :: tab = '  '              !! for indenting
    character(len=*),parameter :: newline = new_line(' ') !! line break character

    if (me%n == 0) return

    str = 'digraph G {'//newline//newline
    if (present(rankdir)) &
        str = str//tab//'rankdir='//rankdir//newline//newline
    if (present(dpi)) &
        str = str//tab//'graph [ dpi = '//integer_to_string(dpi)//' ]'//newline//newline

    ! define the vertices:
    do i=1,me%n
        attributes = get_attributes_string(me%vertices(i)%label, &
                                           me%vertices(i)%attributes)
        str = str//tab//integer_to_string(i)//' '//attributes//newline
        if (i==me%n) str = str//newline
    end do

    ! define the dependencies:
    do i=1,me%n
        if (allocated(me%vertices(i)%edges)) then
            n_edges = size(me%vertices(i)%edges)

            ! if none of the edges have attributes,
            ! then we can write them all on one line
            ! otherwise, write them line by line
            compress = .true.
            do j = 1, n_edges
                if (allocated(me%vertices(i)%edges(j)%label) .or. &
                    allocated(me%vertices(i)%edges(j)%attributes)) then
                    compress = .false.
                    exit
                end if
            end do
            if (.not. compress) then
                ! Example:   1 -> 2 [penwidth=2, arrowhead=none]
                do j=1,n_edges
                    attributes = get_attributes_string(me%vertices(i)%edges(j)%label, &
                                                       me%vertices(i)%edges(j)%attributes)
                    str = str//tab//integer_to_string(i)//' -> '//&
                            integer_to_string(me%vertices(i)%edges(j)%ivertex)//' '//attributes//newline
                end do
            else
                ! Example:   1 -> 2,5,10
                str = str//tab//integer_to_string(i)// merge(' -> ','    ',n_edges/=0)
                do j=1,n_edges
                    ! comma-separated list:
                    str = str//integer_to_string(me%vertices(i)%edges(j)%ivertex)
                    if (n_edges>1 .and. j<n_edges) str = str//','
                end do
                str = str//';'//newline
            end if

        end if
    end do

    str = str//newline//'}'

    contains

        function get_attributes_string(label, attributes) result(str)
            !! create the full attributes string for an edge or node.
            character(len=:),allocatable,intent(in) :: label !! if not allocated or blank, then not used
            character(len=:),allocatable,intent(in) :: attributes !! if not allocated or blank, then not used
            character(len=:),allocatable :: str !! the attributes string, enclosed in brackets

            character(len=:),allocatable :: tmp_label
            logical :: has_label, has_attributes

            has_label = allocated(label)
            if (has_label) has_label = label /= ''
            if (has_label) tmp_label = 'label="'//trim(adjustl(label))//'"'

            has_attributes = allocated(attributes)
            if (has_attributes) has_attributes = attributes /= ''

            if (has_label .and. has_attributes) then
                str = '['//trim(adjustl(attributes))//','//tmp_label//']'
            elseif (has_label .and. .not. has_attributes) then
                str = '['//tmp_label//']'
            elseif (.not. has_label .and. has_attributes) then
                str = '['//trim(adjustl(attributes))//']'
            else ! neither
                str = ''
            end if
        end function get_attributes_string

    end function dag_generate_digraph
!*******************************************************************************

!*******************************************************************************
!>
!  Generate the dependency matrix for the DAG.
!
!  This is an \(n \times n \) matrix with elements \(A_{ij}\),
!  such that \(A_{ij}\) is true if vertex \(i\) depends on vertex \(j\).

    subroutine dag_generate_dependency_matrix(me,mat)

    class(dag),intent(in) :: me
    logical,dimension(:,:),intent(out),allocatable :: mat !! dependency matrix

    integer(ip) :: i !! vertex counter
    integer(ip) :: j !! edge counter

    if (me%n > 0) then

        allocate(mat(me%n,me%n))
        mat = .false.

        do i=1,me%n
            if (allocated(me%vertices(i)%edges)) then
                do j = 1, size(me%vertices(i)%edges)
                    mat(i,me%vertices(i)%edges(j)%ivertex) = .true.
                end do
            end if
        end do

    end if

    end subroutine dag_generate_dependency_matrix
!*******************************************************************************

!*******************************************************************************
!>
!  Generate a Graphviz digraph structure for the DAG and write it to a file.

    subroutine dag_save_digraph(me,filename,rankdir,dpi)

    class(dag),intent(in) :: me
    character(len=*),intent(in),optional :: filename !! file name for diagraph
    character(len=*),intent(in),optional :: rankdir !! right to left orientation (e.g. 'RL')
    integer(ip),intent(in),optional :: dpi !! resolution (e.g. 300)

    integer(ip) :: iunit, istat
    character(len=:),allocatable :: diagraph

    diagraph = me%generate_digraph(rankdir,dpi)

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat)

    if (istat==0) then
        write(iunit,fmt='(A)',iostat=istat) diagraph
    else
        write(*,*) 'error opening '//trim(filename)
    end if

    close(iunit,iostat=istat)

    end subroutine dag_save_digraph
!*******************************************************************************

!*******************************************************************************
!>
!  Integer to allocatable string.

    pure function integer_to_string(i) result(s)

    integer(ip),intent(in) :: i
    character(len=:),allocatable :: s

    integer(ip) :: istat

    allocate( character(len=MAX_INT_STR_LEN) :: s ) ! should be big enough
    write(s,fmt='(ss,I0)',iostat=istat) i
    if (istat==0) then
        s = trim(adjustl(s))
    else
        s = '***'
    end if

    end function integer_to_string
!*******************************************************************************

!*******************************************************************************
!>
!  Return only the unique values from `vec`.
!  The result is also sorted by ascending value.

    function unique(vec) result(vec_unique)

    type(edge),dimension(:),intent(in) :: vec
    type(edge),dimension(:),allocatable :: vec_unique !! only the unique elements of `vec`

    integer(ip) :: i !! counter
    integer(ip) :: n !! size of `vec`
    logical,dimension(:),allocatable :: mask !! for flagging the unique values

    n = size(vec)
    vec_unique = vec  ! make a copy
    if (n<=1) return

    ! get the unique elements by sorting the array
    ! and then excluding any that are the same as the previous element.
    call sort_ascending(vec_unique)
    allocate(mask(n)); mask(1) = .true.
    do i = 2, n
        mask(i) = (vec_unique(i)%ivertex/=vec_unique(i-1)%ivertex)
    end do
    vec_unique = pack(vec_unique, mask)

    end function unique
!*******************************************************************************

!*******************************************************************************
!>
!  Sorts an [[edge]] array `ivec` in increasing order by vertex number.
!  Uses a basic recursive quicksort
!  (with insertion sort for partitions with \(\le\) 20 elements).

    subroutine sort_ascending(ivec)

    type(edge),dimension(:),intent(inout) :: ivec

    integer(ip),parameter :: max_size_for_insertion_sort = 20_ip !! max size for using insertion sort.

    call quicksort(1_ip,size(ivec,kind=ip))

    contains

        recursive subroutine quicksort(ilow,ihigh)

        !! Sort the array

        integer(ip),intent(in) :: ilow
        integer(ip),intent(in) :: ihigh

        integer(ip) :: ipivot !! pivot element
        integer(ip) :: i      !! counter
        integer(ip) :: j      !! counter

        if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

            ! do insertion sort:
            do i = ilow + 1,ihigh
                do j = i,ilow + 1,-1
                    if ( ivec(j)%ivertex < ivec(j-1)%ivertex ) then
                        call swap(ivec(j),ivec(j-1))
                    else
                        exit
                    end if
                end do
            end do

        elseif ( ihigh-ilow>max_size_for_insertion_sort ) then

            ! do the normal quicksort:
            call partition(ilow,ihigh,ipivot)
            call quicksort(ilow,ipivot - 1_ip)
            call quicksort(ipivot + 1_ip,ihigh)

        end if

        end subroutine quicksort

        subroutine partition(ilow,ihigh,ipivot)

        !! Partition the array, based on the
        !! lexical ivecing comparison.

        integer(ip),intent(in)  :: ilow
        integer(ip),intent(in)  :: ihigh
        integer(ip),intent(out) :: ipivot

        integer(ip) :: i,ii

        call swap(ivec(ilow),ivec((ilow+ihigh)/2))
        ii = ilow
        do i = ilow + 1, ihigh
            if ( ivec(i)%ivertex < ivec(ilow)%ivertex ) then
                ii = ii + 1
                call swap(ivec(ii),ivec(i))
            end if
        end do
        call swap(ivec(ilow),ivec(ii))
        ipivot = ii

        end subroutine partition

    end subroutine sort_ascending
!*******************************************************************************

!*******************************************************************************
!>
!  Swap two [[edge]] values.

    pure elemental subroutine swap(i1,i2)

    type(edge),intent(inout) :: i1
    type(edge),intent(inout) :: i2

    type(edge) :: tmp

    tmp = i1
    i1  = i2
    i2  = tmp

    end subroutine swap
!*******************************************************************************

!*******************************************************************************
    end module dag_module
!*******************************************************************************
