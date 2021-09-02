submodule(dag_interface) dag_implementation
  use assert_m, only : assert
  use jsonff, only: &
      fallible_json_string_t, &
      fallible_json_value_t, &
      json_array_t, &
      json_element_t, &
      json_string_t, &
      parse_json
  use erloff, only : error_list_t
  use iso_fortran_env, only: iostat_end
  use iso_varying_string, only : varying_string, operator(//), char, get, put

  implicit none

contains

!*******************************************************************************

   module procedure to_json
     type(error_list_t) :: errors
     type(fallible_json_string_t) :: maybe_key
     type(json_string_t) :: vertices_key
     type(json_array_t) vertices_value

     vertices_value = json_array_t(json_element_t(me%vertices%to_json()))
     maybe_key = fallible_json_string_t("vertices")
     errors = maybe_key%errors()
     call assert(.not. errors%has_any(), "dag%to_json: .not. errors%has_any()", char(errors%to_string()))
     vertices_key = maybe_key%string()
     me_json = json_object_t([vertices_key], [json_element_t(vertices_value)])
   end procedure

   module procedure from_json
     type(vertex_t) :: dag_vertex
     type(error_list_t) :: errors
     integer :: i
     type(fallible_json_value_t) :: maybe_vertices
     type(fallible_json_value_t) :: maybe_vertex
     type(json_element_t) :: vertex_element
     type(json_element_t) :: vertices_element

     maybe_vertices = me_json%get_element("vertices")
     errors = maybe_vertices%errors()
     call assert(.not. errors%has_any(), "dag%from_json: .not. errors%has_any()", char(errors%to_string()))
     select type (vertices => maybe_vertices%value_())
     type is (json_array_t)
       call me%set_vertices(vertices%length())
       do i = 1, vertices%length()
         maybe_vertex = vertices%get_element(i)
         errors = maybe_vertex%errors()
         call assert(.not. errors%has_any(), "dag%from_json: .not. errors%has_any()", char(errors%to_string()))
         select type (vertex_json => maybe_vertex%value_())
         type is (json_object_t)
           dag_vertex = vertex_t(vertex_json)
           call me%set_edges(i, dag_vertex%edges)
         class default
           call assert(.false., "dag%from_json: vertex was not an object", char(vertex_json%to_compact_string()))
         end select
       end do
     class default
       call assert(.false., "dag%from_json: vertices was not an array", char(vertices%to_compact_string()))
     end select
   end procedure

!*******************************************************************************

  module procedure dag_get_num_vertices
    num_vertices = size(me%vertices)
  end procedure

  module procedure dag_get_edges

    call assert(ivertex>=lbound(me%vertices,1) .and. ivertex<=ubound(me%vertices,1),"dag_get_edges: index in bounds")
    edges = me%vertices(ivertex)%edges

  end procedure

!*******************************************************************************

  module procedure dag_get_dependencies

    integer :: i

    call assert(ivertex>=lbound(me%vertices,1) .and. ivertex<=ubound(me%vertices,1),"dag_get_dependencies: index in bounds")

    allocate(dep(0))

    do i=1, size(me%vertices)
      if (allocated(me%vertices(i)%edges)) then
        if (any(me%vertices(i)%edges == ivertex)) dep = [dep, i]
      end if
    end do

  end procedure

!*******************************************************************************

  module procedure dag_set_vertices

    integer :: i

    allocate(me%vertices(nvertices))
    call me%vertices%set_vertex_id( [(i,i=1,nvertices)] )

  end procedure

!*******************************************************************************
  module procedure set_vertex_label
    call me%vertices(ivertex)%set_label(label)
  end procedure
!*******************************************************************************
  module procedure set_vertex_attributes
    call me%vertices(ivertex)%set_attributes(attributes)
  end procedure
!*******************************************************************************

  module procedure dag_set_vertex_info

    if (present(label)) then
        call me%vertices(ivertex)%set_label(label)
    else
        call me%vertices(ivertex)%set_label(integer_to_string(ivertex))
    end if

    if (present(attributes)) call me%vertices(ivertex)%set_attributes(attributes)

  end procedure

!*******************************************************************************

  module procedure dag_set_edges

    call me%vertices(ivertex)%set_edges(edges)

  end procedure

!*******************************************************************************

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

  contains

!*******************************************************************************

    recursive subroutine dfs(v)

      type(vertex_t),intent(inout) :: v
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

  end procedure dag_toposort

!*******************************************************************************

  module procedure dag_generate_digraph

    integer :: i,j
    integer :: n_edges
    character(len=:),allocatable :: attributes, label

    character(len=*),parameter :: tab = '  '
    character(len=*),parameter :: newline = new_line(' ')

    str = 'digraph G {'//newline//newline
    if (present(rankdir)) &
        str = str//tab//'rankdir='//rankdir//newline//newline
    if (present(dpi)) &
        str = str//tab//'graph [ dpi = '//integer_to_string(dpi)//' ]'//newline//newline

    block
      logical, parameter :: temporary_debugging=.true.

      if (temporary_debugging) print *,"size(me%vertices) ", size(me%vertices)
    end block

    ! define the vertices:
    do i=1,size(me%vertices)
      associate( &
        has_label      => me%vertices(i)%has_label(), &
        has_attributes => me%vertices(i)%has_attributes() &
      )

        block
          logical, parameter :: temporary_debugging=.true.

          if (temporary_debugging) print *,"vertex ",i," has label ", trim(adjustl(me%vertices(i)%get_label()))
        end block

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

!*******************************************************************************

  module procedure dag_generate_dependency_matrix

    integer :: i

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

!*******************************************************************************

  module procedure dag_save_digraph

    integer :: iunit, istat
    character(len=:),allocatable :: diagraph

    print *,"dag_dave_digraph: calling generate_digraph"
    diagraph = me%generate_digraph(rankdir,dpi)
    print *,"dag_dave_digraph: generate_digraph completed"

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat)

    if (istat==0) then
        write(iunit,fmt='(A)',iostat=istat) diagraph
    else
        write(*,*) 'error opening '//trim(filename)
    end if

    close(iunit,iostat=istat)

  end procedure

!*******************************************************************************

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

!*******************************************************************************

  module procedure read_formatted

    character(len=*), parameter :: NEWLINE = NEW_LINE('A')
    type(varying_string) :: contents
    type(error_list_t) :: errors
    type(fallible_json_value_t) :: maybe_json
    type(dag_t) :: me_local
    type(varying_string) :: tmp

    call get(unit, contents, iostat = iostat)
    if (iostat == iostat_end) return
    do
      call get(unit, tmp, iostat = iostat)
      if (iostat == iostat_end) exit
      contents = contents // NEWLINE // tmp
    end do

    maybe_json = parse_json(contents)
    errors = maybe_json%errors()
    call assert(.not. errors%has_any(), "dag%read_formatted: .not. errors%has_any()", char(errors%to_string()))

    select type (object => maybe_json%value_())
    type is (json_object_t)
      me_local = from_json(object)
      me%vertices = me_local%vertices
      ! call assert(me%defined(), me%error_message)
    class default
      call assert(.false., "dag%read_formatted: didn't get a json object")
    end select

  end procedure

!*******************************************************************************

  module procedure write_formatted

    type(json_object_t) :: me_json

    me_json = me%to_json()
    write(unit,*) char(me_json%to_expanded_string())

  end procedure

!*******************************************************************************

end submodule dag_implementation
