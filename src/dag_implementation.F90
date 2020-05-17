submodule(dag_interface) dag_implementation
  use assert_interface, only : assert
  implicit none

contains

  module procedure dag_get_edges

    call assert(ivertex>=lbound(me%vertices,1) .and. ivertex<=ubound(me%vertices,1),"dag_get_edges: index in bounds")
    edges = me%vertices(ivertex)%edges

  end procedure

  module procedure dag_get_dependencies

    integer :: i !! vertex counter

    call assert(ivertex>=lbound(me%vertices,1) .and. ivertex<=ubound(me%vertices,1),"dag_get_dependencies: index in bounds")

    allocate(dep(0))

    do i=1, size(me%vertices)
      if (allocated(me%vertices(i)%edges)) then
        if (any(me%vertices(i)%edges == ivertex)) dep = [dep, i]
      end if
    end do

  end procedure

  module procedure dag_set_vertices

    integer :: i

    allocate(me%vertices(nvertices))
    call me%vertices%set_vertex_id( [(i,i=1,nvertices)] )

  end procedure

  module procedure dag_set_vertex_info

    if (present(label)) then
        call me%vertices(ivertex)%set_label(label)
    else
        ! just use the vertex number
        call me%vertices(ivertex)%set_label(integer_to_string(ivertex))
    end if

    if (present(attributes)) call me%vertices(ivertex)%set_attributes(attributes)

  end procedure

  module procedure dag_set_edges

    call me%vertices(ivertex)%set_edges(edges)

  end procedure

  module procedure dag_toposort

    integer :: i,iorder

    associate( num_vertices => size(me%vertices))
      if (num_vertices==0) return

      allocate(order(num_vertices))

      iorder = 0  ! index in order array
      istat = 0   ! no errors so far
      do i=1,num_vertices
        if (.not. me%vertices(i)%get_marked()) call dfs(me%vertices(i))
        if (istat==-1) exit
      end do
    end associate

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

      associate( v_checking => v%get_checking(), v_marked => v%get_marked())
        if (v_checking) then
          ! error: circular dependency
          istat = -1
        else
          if (.not. v_marked) then
            call v%set_checking(.true.)
            if (allocated(v%edges)) then
              do j=1,size(v%edges)
                call dfs(me%vertices(v%edges(j)))
                if (istat==-1) return
              end do
            end if
            call v%set_checking(.false.)
            call v%set_marked(.true.)
            iorder = iorder + 1
            order(iorder) = v%get_vertex_id()
          end if
        end if
      end associate

    end subroutine dfs

#ifndef FORD
  end procedure dag_toposort
#endif

  module procedure dag_generate_digraph

    integer :: i,j     !! counter
    integer :: n_edges !! number of edges
    character(len=:),allocatable :: attributes, label

    character(len=*),parameter :: tab = '  '               !! for indenting
    character(len=*),parameter :: newline = new_line(' ')  !! line break character

    str = 'digraph G {'//newline//newline
    if (present(rankdir)) &
        str = str//tab//'rankdir='//rankdir//newline//newline
    if (present(dpi)) &
        str = str//tab//'graph [ dpi = '//integer_to_string(dpi)//' ]'//newline//newline

    ! define the vertices:
    do i=1,size(me%vertices)
      associate( &
        has_label      => me%vertices(i)%has_label(), &
        has_attributes => me%vertices(i)%has_attributes() &
      )
        if (has_label) label = 'label="'//trim(adjustl(me%vertices(i)%get_label()))//'"'
        if (has_label .and. has_attributes) then
            attributes = '['//trim(adjustl(me%vertices(i)%get_attributes()))//','//label//']'
        elseif (has_label .and. .not. has_attributes) then
            attributes = '['//label//']'
        elseif (.not. has_label .and. has_attributes) then
            attributes = '['//trim(adjustl(me%vertices(i)%get_attributes()))//']'
        else ! neither
            attributes = ''
        end if
      end associate
      str = str//tab//integer_to_string(i)//' '//attributes//newline
      if (i==size(me%vertices)) str = str//newline
    end do

    ! define the dependencies:
    do i=1,size(me%vertices)
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

  module procedure dag_generate_dependency_matrix

    integer :: i !! vertex counter

    associate(num_vertices => size(me%vertices))
      if (num_vertices > 0) then

        allocate(mat(num_vertices,num_vertices))
        mat = .false.

        do i=1,num_vertices
            if (allocated(me%vertices(i)%edges)) then
                mat(i,me%vertices(i)%edges) = .true.
            end if
        end do

      end if
    end associate

  end procedure

  module procedure dag_save_digraph

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

  pure function integer_to_string(i) result(s)

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

  module procedure output
    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit
    type(json_value), pointer :: root, array, element
    type(json_core) :: json
    integer(IK) :: iunit, i, error_cnt
    logical(LK) :: is_valid
    character(kind=json_CK,len=:), allocatable :: error_msg

    call json%initialize()
    call terminate_if_error(json, operation="initialize()")

    call json%create_object(root,'') ! create root object
    call terminate_if_error(json, operation="create_object()")

    call json%create_array(array,'')
    call increment_if_error(json, error_cnt)

    call json%add_by_path(root, 'dag.vertices', array)
    call increment_if_error(json, error_cnt)

    do i=1,size(me%vertices)
      call json%create_object(element,'')
      call increment_if_error(json, error_cnt)

      call json%add(element, 'edges', me%vertices(i)%get_edges())
      if (json%failed()) then
          call json%print_error_message(error_unit)
          error_cnt = error_cnt + 1
      end if

      call json%add(element, 'checking', me%vertices(i)%get_checking())
      if (json%failed()) then
          call json%print_error_message(error_unit)
          error_cnt = error_cnt + 1
      end if

      call json%add(element, 'marked', me%vertices(i)%get_marked())
      if (json%failed()) then
          call json%print_error_message(error_unit)
          error_cnt = error_cnt + 1
      end if

      call json%add(element, 'label', me%vertices(i)%get_label())
      if (json%failed()) then
          call json%print_error_message(error_unit)
          error_cnt = error_cnt + 1
      end if

      call json%add(element, 'attributes', me%vertices(i)%get_attributes())
      if (json%failed()) then
          call json%print_error_message(error_unit)
          error_cnt = error_cnt + 1
      end if

      call json%add(array, element)
      call increment_if_error(json, error_cnt)
    end do

    call json%validate(root,is_valid,error_msg)
    if (.not. is_valid) then
        write(error_unit,'(A)') 'Error: root is not a valid JSON linked list: '//error_msg
        error_cnt = error_cnt + 1
    end if

    open(newunit=iunit, file=filename, status='UNKNOWN')
    call json%print(root,iunit)
    call increment_if_error(json, error_cnt)
    close(iunit)

    call json%destroy(root)
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

    subroutine add_intrinsic_variable(json, variable, variable_name, error_cnt, var)
      type(json_core),intent(inout) :: json
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
        rank default
          error stop "dag add_intrinsic_variable: rank " // integer_to_string(rank(variable)) // " unsupported."
      end select

      if (json%failed()) then
          call json%print_error_message(error_unit)
          error_cnt = error_cnt + 1
      end if

    end subroutine

  end procedure

end submodule dag_implementation
