!*******************************************************************************
!>
!  DAG Module.

    module dag_module

    implicit none

    private

    type :: edge
        !! the "to" vertex that defines an edge. This is part of
        !! the array of vertices contained without the "from" [[vertex]] type.
        !! an edge can also have optional attrubutes for graphviz.
        integer :: ivertex = 0 !! vertex number (the index in the [[dag]] `vertices` array)
        character(len=:),allocatable :: label !! used for diagraph
        character(len=:),allocatable :: attributes !! used for diagraph
    end type edge

    type :: vertex
        !! a vertex of a directed acyclic graph (DAG)
        private
        type(edge),dimension(:),allocatable :: edges  !! these are the vertices that this vertex
                                                      !! depends on. (edges of the graph).
        integer :: ivertex = 0 !! vertex number (the index in the [[dag]] `vertices` array)
        logical :: checking = .false.  !! used for toposort
        logical :: marked = .false.    !! used for toposort
        character(len=:),allocatable :: label      !! used for diagraph
        character(len=:),allocatable :: attributes !! used for diagraph
    contains
        private
        generic :: set_edges => set_edge_vector_vector, add_edge
        procedure :: set_edge_vector_vector, add_edge
    end type vertex

    type,public :: dag
        !! a directed acyclic graph (DAG)
        private
        integer :: n = 0 !! number of vertices (size of `vertices` array)
        type(vertex),dimension(:),allocatable :: vertices  !! the vertices in the DAG. The index in
                                                           !! this array if the vertex number.
    contains
        private
        procedure,public :: vertex             => dag_get_vertex ! not very useful for now, since all vertex attributes are private
        procedure,public :: number_of_vertices => dag_get_number_of_vertices
        procedure,public :: set_vertices       => dag_set_vertices

        !procedure,public :: set_edges          => dag_set_edges
        generic,public :: set_edges => dag_set_edges_no_atts, dag_set_edges_vector_atts
        procedure :: dag_set_edges_no_atts, dag_set_edges_vector_atts

        procedure,public :: set_vertex_info    => dag_set_vertex_info
        procedure,public :: toposort           => dag_toposort
        procedure,public :: generate_digraph   => dag_generate_digraph
        procedure,public :: generate_dependency_matrix => dag_generate_dependency_matrix
        procedure,public :: save_digraph       => dag_save_digraph
        procedure,public :: get_edges          => dag_get_edges
        procedure,public :: get_dependencies   => dag_get_dependencies
        procedure,public :: destroy            => dag_destroy
        procedure :: init_internal_vars !! private routine to initialize some internal variables
    end type dag

    contains
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

    subroutine set_edge_vector_vector(me,edges,label,attributes)

    class(vertex),intent(inout) :: me
    integer,dimension(:),intent(in) :: edges
    character(len=*),dimension(:),intent(in),optional :: label
    character(len=*),dimension(:),intent(in),optional :: attributes !! other attributes when
                                                                    !! saving as a diagraph.

    integer :: i !! counter

    do i=1,size(edges)
        if (present(label) .and. present(attributes)) then
            call me%add_edge(edges(i),label=label(i),attributes=attributes(i))
        else if (.not. present(label) .and. present(attributes)) then
            call me%add_edge(edges(i),attributes=attributes(i))
        else if (present(label) .and. .not. present(attributes)) then
            call me%add_edge(edges(i),label=label(i))
        else
            call me%add_edge(edges(i))
        end if
    end do

    end subroutine set_edge_vector_vector
!*******************************************************************************

!*******************************************************************************
!>
!  add an edge index for this vertex

    subroutine add_edge(me,e,label,attributes)

    class(vertex),intent(inout) :: me
    integer,intent(in) :: e
    character(len=*),intent(in),optional :: label
    character(len=*),intent(in),optional :: attributes !! other attributes when
                                                       !! saving as a diagraph.

    if (allocated(me%edges)) then
        if (.not. any(e==me%edges%ivertex)) then ! don't add if already there

            ! me%edges = [me%edges, edge(e,label=label,attributes=attributes)]
            if (present(label) .and. present(attributes)) then
                me%edges = [me%edges, edge(e,label=label,attributes=attributes)]
            else if (.not. present(label) .and. present(attributes)) then
                me%edges = [me%edges, edge(e,attributes=attributes)]
            else if (present(label) .and. .not. present(attributes)) then
                me%edges = [me%edges, edge(e,label=label)]
            else
                me%edges = [me%edges, edge(e)]
            end if
            call sort_ascending(me%edges)
        end if
    else
        allocate(me%edges(1))
        ! me%edges = [edge(e,label=label,attributes=attributes)]
        if (present(label) .and. present(attributes)) then
            me%edges = [edge(e,label=label,attributes=attributes)]
        else if (.not. present(label) .and. present(attributes)) then
            me%edges = [edge(e,attributes=attributes)]
        else if (present(label) .and. .not. present(attributes)) then
            me%edges = [edge(e,label=label)]
        else
            me%edges = [edge(e)]
        end if
    end if

    end subroutine add_edge
!*******************************************************************************

!*******************************************************************************
!>
!  get the edges for the vertex (all of the vertices
!  that this vertex depends on).

    pure function dag_get_edges(me,ivertex) result(edges)

    implicit none

    class(dag),intent(in) :: me
    integer,intent(in) :: ivertex
    integer,dimension(:),allocatable :: edges

    if (ivertex>0 .and. ivertex <= me%n) then
        edges = me%vertices(ivertex)%edges%ivertex  ! auto LHS allocation
    end if

    end function dag_get_edges
!*******************************************************************************

!*******************************************************************************
!>
!  get all the vertices that depend on this vertex.

    pure function dag_get_dependencies(me,ivertex) result(dep)

    implicit none

    class(dag),intent(in) :: me
    integer,intent(in) :: ivertex
    integer,dimension(:),allocatable :: dep  !! the set of all vertices
                                             !! than depend on `ivertex`

    integer :: i !! vertex counter

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
!  set the number of vertices in the dag

    subroutine dag_set_vertices(me,nvertices,labels)

    class(dag),intent(inout) :: me
    integer,intent(in)       :: nvertices !! number of vertices
    character(len=*),dimension(nvertices),intent(in),optional :: labels !! vertex name strings
    integer :: i !! counter

    if (allocated(me%vertices)) deallocate(me%vertices)

    me%n = nvertices
    allocate(me%vertices(nvertices))
    me%vertices%ivertex = [(i,i=1,nvertices)]

    if (present(labels)) then
        do i = 1, nvertices
            me%vertices(i)%label = trim(adjustl(labels(i)))
        end do
    else
        ! just use the vertex number
        do i = 1, nvertices
            me%vertices(i)%label = integer_to_string(i)
        end do
    end if

    end subroutine dag_set_vertices
!*******************************************************************************

!*******************************************************************************
!>
!  Returns the number of vertices in the dag.

    pure function dag_get_number_of_vertices(me) result(nvertices)

    class(dag),intent(in) :: me
    integer :: nvertices !! number of vertices

    nvertices = me%n

    end function dag_get_number_of_vertices
!*******************************************************************************

!*******************************************************************************
!>
!  set info about a vertex in a dag.

    subroutine dag_set_vertex_info(me,ivertex,label,attributes)

    class(dag),intent(inout) :: me
    integer,intent(in)                   :: ivertex !! vertex number
    character(len=*),intent(in),optional :: label !! if a label is not set,
                                                  !! then the integer vertex
                                                  !! number is used.
    character(len=*),intent(in),optional :: attributes !! other attributes when
                                                       !! saving as a diagraph.

    if (present(label)) me%vertices(ivertex)%label = label
    if (present(attributes)) me%vertices(ivertex)%attributes = attributes

    end subroutine dag_set_vertex_info
!*******************************************************************************

!*******************************************************************************
!>
!  Get the `i`th vertex.
!
!  The program will stop if vertex `i` does not exist.

    function dag_get_vertex(me,i) result(v)

    class(dag),intent(inout) :: me
    integer,intent(in) :: i !! vertex number
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
!  set the edges for a vertex in a dag

    subroutine dag_set_edges_no_atts(me,ivertex,edges)

    class(dag),intent(inout)        :: me
    integer,intent(in)              :: ivertex !! vertex number
    integer,dimension(:),intent(in) :: edges

    call me%vertices(ivertex)%set_edges(edges)

    end subroutine dag_set_edges_no_atts
!*******************************************************************************

!*******************************************************************************
!>
!  set the edges for a vertex in a dag

    subroutine dag_set_edges_vector_atts(me,ivertex,edges,attributes,label)

    class(dag),intent(inout)        :: me
    integer,intent(in)              :: ivertex !! vertex number
    integer,dimension(:),intent(in) :: edges
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

    integer :: i !! counter

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
    integer,dimension(:),allocatable,intent(out) :: order  !! the toposort order
    integer,intent(out) :: istat !! Status flag:
                                 !!
                                 !! * 0 if no errors
                                 !! * -1 if circular dependency
                                 !!  (in this case, `order` will not be allocated)

    integer :: i,iorder

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
    integer :: j

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
!  Generate a Graphviz digraph structure for the DAG.
!
!### Example
!  * To convert this to a PDF using `dot`: `dot -Tpdf -o test.pdf test.dot`,
!    where `test.dot` is `str` written to a file.

    function dag_generate_digraph(me,rankdir,dpi) result(str)

    implicit none

    class(dag),intent(in) :: me
    character(len=:),allocatable :: str
    character(len=*),intent(in),optional :: rankdir !! right to left orientation (e.g. 'RL')
    integer,intent(in),optional :: dpi !! resolution (e.g. 300)

    integer :: i,j     !! counter
    integer :: n_edges !! number of edges
    character(len=:),allocatable :: attributes,label
    logical :: has_label, has_attributes, compress

    character(len=*),parameter :: tab = '  '               !! for indenting
    character(len=*),parameter :: newline = new_line(' ')  !! line break character

    if (me%n == 0) return

    str = 'digraph G {'//newline//newline
    if (present(rankdir)) &
        str = str//tab//'rankdir='//rankdir//newline//newline
    if (present(dpi)) &
        str = str//tab//'graph [ dpi = '//integer_to_string(dpi)//' ]'//newline//newline

    ! define the vertices:
    do i=1,me%n
        has_label      = allocated(me%vertices(i)%label)
        has_attributes = allocated(me%vertices(i)%attributes)
        if (has_label) label = 'label="'//trim(adjustl(me%vertices(i)%label))//'"'
        if (has_label .and. has_attributes) then
            attributes = '['//trim(adjustl(me%vertices(i)%attributes))//','//label//']'
        elseif (has_label .and. .not. has_attributes) then
            attributes = '['//label//']'
        elseif (.not. has_label .and. has_attributes) then
            attributes = '['//trim(adjustl(me%vertices(i)%attributes))//']'
        else ! neither
            attributes = ''
        end if
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
                if (allocated(me%vertices(i)%edges(j)%attributes)) then  ! label not used yet TODO
                    write(*,*) 'attribute: ', me%vertices(i)%edges(j)%attributes
                    compress = .false.
                    exit
                end if
            end do
            if (.not. compress) then
                ! Example:   1 -> 2 [penwidth=2, arrowhead=none]
                do j=1,n_edges
                    has_label      = allocated(me%vertices(i)%edges(j)%label)
                    has_attributes = allocated(me%vertices(i)%edges(j)%attributes)
                    if (has_label) label = 'label="'//trim(adjustl(me%vertices(i)%edges(j)%label))//'"'
                    if (has_label .and. has_attributes) then
                        attributes = '['//trim(adjustl(me%vertices(i)%edges(j)%attributes))//','//label//']'
                    elseif (has_label .and. .not. has_attributes) then
                        attributes = '['//label//']'
                    elseif (.not. has_label .and. has_attributes) then
                        attributes = '['//trim(adjustl(me%vertices(i)%edges(j)%attributes))//']'
                    else ! neither
                        attributes = ''
                    end if
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

    end function dag_generate_digraph
!*******************************************************************************

!*******************************************************************************
!>
!  Generate the dependency matrix for the DAG.
!
!  This is an \(n \times n \) matrix with elements \(A_{ij}\),
!  such that \(A_{ij}\) is true if vertex \(i\) depends on vertex \(j\).

    subroutine dag_generate_dependency_matrix(me,mat)

    implicit none

    class(dag),intent(in) :: me
    logical,dimension(:,:),intent(out),allocatable :: mat !! dependency matrix

    integer :: i !! vertex counter
    integer :: j !! edge counter

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

    implicit none

    class(dag),intent(in) :: me
    character(len=*),intent(in),optional :: filename !! file name for diagraph
    character(len=*),intent(in),optional :: rankdir !! right to left orientation (e.g. 'RL')
    integer,intent(in),optional :: dpi !! resolution (e.g. 300)

    integer :: iunit, istat
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

    implicit none

    integer,intent(in) :: i
    character(len=:),allocatable :: s

    integer :: istat

    allocate( character(len=64) :: s )  ! should be big enough
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

    integer :: i !! counter
    integer :: n !! size of `vec`
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
!  Sorts an integer array `ivec` in increasing order.
!  Uses a basic recursive quicksort
!  (with insertion sort for partitions with \(\le\) 20 elements).

    subroutine sort_ascending(ivec)

    type(edge),dimension(:),intent(inout) :: ivec

    integer,parameter :: max_size_for_insertion_sort = 20 !! max size for using insertion sort.

    call quicksort(1,size(ivec))

    contains

        recursive subroutine quicksort(ilow,ihigh)

        !! Sort the array

        integer,intent(in) :: ilow
        integer,intent(in) :: ihigh

        integer :: ipivot !! pivot element
        integer :: i      !! counter
        integer :: j      !! counter

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
            call quicksort(ilow,ipivot - 1)
            call quicksort(ipivot + 1,ihigh)

        end if

        end subroutine quicksort

        subroutine partition(ilow,ihigh,ipivot)

        !! Partition the array, based on the
        !! lexical ivecing comparison.

        implicit none

        integer,intent(in)  :: ilow
        integer,intent(in)  :: ihigh
        integer,intent(out) :: ipivot

        integer :: i,ip

        call swap(ivec(ilow),ivec((ilow+ihigh)/2))
        ip = ilow
        do i = ilow + 1, ihigh
            if ( ivec(i)%ivertex < ivec(ilow)%ivertex ) then
                ip = ip + 1
                call swap(ivec(ip),ivec(i))
            end if
        end do
        call swap(ivec(ilow),ivec(ip))
        ipivot = ip

        end subroutine partition

    end subroutine sort_ascending
!*******************************************************************************

!*******************************************************************************
!>
!  Swap two integer values.

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
