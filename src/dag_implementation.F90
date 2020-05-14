!*******************************************************************************
!>
!  DAG Module.

    submodule(dag_interface) dag_implementation

    implicit none

contains

!*******************************************************************************
!>
!  get the edges for the vertex (all the the vertices
!  that this vertex depends on).

    module procedure dag_get_edges

    if (ivertex>0 .and. ivertex <= me%n) then
        edges = me%vertices(ivertex)%edges  ! auto LHS allocation
    end if

    end procedure
!*******************************************************************************

!*******************************************************************************
!>
!  get all the vertices that depend on this vertex.

    module procedure dag_get_dependencies

    implicit none

    integer :: i !! vertex counter

    if (ivertex>0 .and. ivertex <= me%n) then

        ! have to check all the vertices:
        do i=1, me%n
            if (allocated(me%vertices(i)%edges)) then
                if (any(me%vertices(i)%edges == ivertex)) then
                    if (allocated(dep)) then
                        dep = [dep, i]  ! auto LHS allocation
                    else
                        dep = [i]       ! auto LHS allocation
                    end if
                end if
            end if
        end do

    end if

    end procedure
!*******************************************************************************

!*******************************************************************************
!>
!  set the number of vertices in the dag

    module procedure dag_set_vertices

    integer :: i

    me%n = nvertices
    allocate(me%vertices(nvertices))
    me%vertices%ivertex = [(i,i=1,nvertices)]

    end procedure
!*******************************************************************************

!*******************************************************************************
!>
!  set info about a vertex in a dag.

    module procedure dag_set_vertex_info

    if (present(label)) then
        me%vertices(ivertex)%label = label
    else
        ! just use the vertex number
        me%vertices(ivertex)%label = integer_to_string(ivertex)
    end if

    if (present(attributes)) then
        me%vertices(ivertex)%attributes = attributes
    end if

    end procedure
!*******************************************************************************

!*******************************************************************************
!>
!  set the edges for a vertex in a dag

    module procedure dag_set_edges

    call me%vertices(ivertex)%set_edges(edges)

    end procedure
!*******************************************************************************

!*******************************************************************************
!>
!  Main toposort routine

    module procedure dag_toposort

    integer :: i,iorder

    if (me%n==0) return

    allocate(order(me%n))

    iorder = 0  ! index in order array
    istat = 0   ! no errors so far
    do i=1,me%n
      if (.not. me%vertices(i)%marked) call dfs(me%vertices(i))
      if (istat==-1) exit
    end do

    if (istat==-1) deallocate(order)

#ifndef FORD
    contains
#else
    end procedure ! work around ford documentation generator bug
#endif

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
            call dfs(me%vertices(v%edges(j)))
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

#ifndef FORD
    end procedure dag_toposort
#endif
!*******************************************************************************

!*******************************************************************************
!>
!  Generate a Graphviz digraph structure for the DAG.
!
!### Example
!  * To convert this to a PDF using `dot`: `dot -Tpdf -o test.pdf test.dot`,
!    where `test.dot` is `str` written to a file.

    module procedure dag_generate_digraph

    implicit none

    integer :: i,j     !! counter
    integer :: n_edges !! number of edges
    character(len=:),allocatable :: attributes,label
    logical :: has_label, has_attributes

    character(len=*),parameter :: tab = '  '               !! for indenting
    character(len=*),parameter :: newline = new_line(' ')  !! line break character

    if (me%n == unset) return

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
            str = str//tab//integer_to_string(i)//merge(' -> ','    ',n_edges/=0)
            do j=1,n_edges
                ! comma-separated list:
                str = str//integer_to_string(me%vertices(i)%edges(j))
                if (n_edges>1 .and. j<n_edges) str = str//','
            end do
            str = str//';'//newline
        end if
    end do

    str = str//newline//'}'

    end procedure dag_generate_digraph
!*******************************************************************************

!*******************************************************************************
!>
!  Generate the dependency matrix for the DAG.
!
!  This is an \(n \times n \) matrix with elements \(A_{ij}\),
!  such that \(A_{ij}\) is true if vertex \(i\) depends on vertex \(j\).

    module procedure dag_generate_dependency_matrix

    implicit none

    integer :: i !! vertex counter

    if (me%n > 0) then

        allocate(mat(me%n,me%n))
        mat = .false.

        do i=1,me%n
            if (allocated(me%vertices(i)%edges)) then
                mat(i,me%vertices(i)%edges) = .true.
            end if
        end do

    end if

    end procedure
!*******************************************************************************

!*******************************************************************************
!>
!  Generate a Graphviz digraph structure for the DAG and write it to a file.

    module procedure dag_save_digraph

    implicit none

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

    end procedure
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
! Write DAG to JSON file
  module procedure output
    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit
    type(json_value), pointer :: p, graph, var
    type(json_core) :: json
    integer(IK) :: iunit, i, error_cnt
    logical(LK) :: is_valid
    character(kind=json_CK,len=:), allocatable :: error_msg

    call json%initialize()
    call terminate_if_error(json, operation="initialize()")

    call json%create_object(p,'') ! create root object
    call terminate_if_error(json, operation="create_object()")

    call json%add_by_path(p, 'dag.n', me%n)
    call increment_if_error(json, error_cnt)

    call json%create_array(graph,'')
    call increment_if_error(json, error_cnt)

    call json%add_by_path(p, 'dag.vertices', graph)
    call increment_if_error(json, error_cnt)

    do i=1,size(me%vertices)
      nullify(var)
      call json%create_object(var,'')
      call increment_if_error(json, error_cnt)

      associate( e => me%vertices(i)%edges(:)) ! strip allocatable attribute (gfortran 10.1 bug workaround)
        call add_intrinsic_variable(json, graph, e, 'edges', error_cnt, var)
      end associate
      call add_intrinsic_variable(json, graph, me%vertices(i)%ivertex, 'ivertex', error_cnt, var)
      call add_intrinsic_variable(json, graph, me%vertices(i)%checking, 'checking', error_cnt, var)
      call add_intrinsic_variable(json, graph, me%vertices(i)%marked, 'marked', error_cnt, var)
      call add_intrinsic_variable(json, graph, me%vertices(i)%label, 'label', error_cnt, var)
      call add_intrinsic_variable(json, graph, me%vertices(i)%attributes, 'attributes', error_cnt, var)

      call json%add(graph, var)
      call increment_if_error(json, error_cnt)
      nullify(var)
    end do
    nullify(graph)

    call json%validate(p,is_valid,error_msg)
    if (.not. is_valid) then
        write(error_unit,'(A)') 'Error: p is not a valid JSON linked list: '//error_msg
        error_cnt = error_cnt + 1
    end if

    open(newunit=iunit, file=filename, status='UNKNOWN')
    call json%print(p,iunit)
    call increment_if_error(json, error_cnt)
    close(iunit)

    call json%destroy(p)
    call increment_if_error(json, error_cnt)

  contains

    subroutine increment_if_error(json_factory, error_count)
      type(json_core), intent(in) :: json_factory
      integer(IK), intent(inout) :: error_count

      if (json_factory%failed()) then
        call json%print_error_message(error_unit)
        error_count = error_count + 1
      end if
    end subroutine

    subroutine terminate_if_error(json_factory, operation)
      type(json_core), intent(inout) :: json_factory
      character(len=*), intent(in) :: operation

      if (json_factory%failed()) then
        call json_factory%print_error_message(error_unit)
        error stop "json%" // operation //" failed."
      end if
    end subroutine

    subroutine add_intrinsic_variable(json, me, variable, variable_name, error_cnt, var)
      type(json_core),intent(inout) :: json
      type(json_value), pointer, intent(inout) :: me
      class(*), intent(in) :: variable(..)
      character(len=*), intent(in) :: variable_name
      integer, intent(inout) :: error_cnt
      type(json_value),pointer, intent(inout) :: var

      select rank(variable)
        rank(0)
          select type(variable)
            type is(integer(IK))
              call json%add(var, variable_name, variable)
            type is(logical)
              call json%add(var, variable_name, variable)
            type is(character(len=*))
              call json%add(var, variable_name, variable)
            class default
              error stop "dag add_intrinsic_variable: unsupported rank-0 type"
          end select
        rank(1)
          select type(variable)
            type is(integer(IK))
              call json%add(var, variable_name, variable)
            class default
              error stop "dag add_intrinsic_variable: unsupported rank-1 type"
          end select
      end select

      if (json%failed()) then
          call json%print_error_message(error_unit)
          error_cnt = error_cnt + 1
      end if

    end subroutine

    end procedure
!*******************************************************************************

    end submodule dag_implementation
!*******************************************************************************
