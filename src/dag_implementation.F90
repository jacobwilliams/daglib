submodule(dag_interface) dag_implementation
  use assert_interface, only : assert
  use yafyaml, only : Parser, Configuration, FileStream
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

  module procedure input
    !! Read a dag from a JSON file
    use gFTL_UnlimitedVector, only : UnlimitedVector

    type(Parser) p
    type(Configuration) c, subconfig

    class(*), pointer :: dag_vertices=>null(), dag_vertices_i_edges=>null()
    integer :: i, j

    p = Parser('core')
    c = p%load(FileStream(filename))
    call c%get_node_at_selector(dag_vertices, 'dag', 'vertices')

    select type (dag_vertices)
    class is (UnlimitedVector)

      allocate(me%vertices(dag_vertices%size()))

      do i=1,size(me%vertices)

        call c%get(subconfig, 'dag', 'vertices', i)
        call subconfig%get_node_at_selector(dag_vertices_i_edges, 'edges')

        select type (dag_vertices_i_edges)
        class is (UnlimitedVector)

          allocate(me%vertices(i)%edges(dag_vertices_i_edges%size()))

          do j = 1,size(me%vertices(i)%edges)
            me%vertices(i)%edges(j) = c%at('dag','vertices',i,'edges',j)
          end do
        class default
          error stop "unexpected dag_vertices_i_edges class"
        end select

      end do

    class default
       error stop "unexpected dag_vertices class"
    end select

  end procedure
end submodule dag_implementation
