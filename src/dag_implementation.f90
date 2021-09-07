submodule(dag_interface) dag_implementation
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use jsonff, only: &
      fallible_json_string_t, &
      fallible_json_value_t, &
      json_array_t, &
      json_element_t, &
      json_string_t, &
      parse_json
  use erloff, only : error_list_t
  use iso_fortran_env, only: iostat_end
  use iso_varying_string, only : varying_string, operator(//), char, get, put, var_str

  implicit none

contains

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

   module procedure construct
     new_dag%vertices = vertices
   end procedure

   module procedure from_json
     type(fallible_json_value_t) :: maybe_vertices
     type(fallible_json_value_t) :: maybe_vertex

     maybe_vertices = me_json%get_element("vertices")
     associate(errors => maybe_vertices%errors())
       call assert(.not. errors%has_any(), "dag%from_json: .not. errors%has_any()", char(errors%to_string()))
     end associate
     select type (vertices => maybe_vertices%value_())
     type is (json_array_t)

       associate(nvertices => vertices%length())
         allocate(me%vertices(nvertices))
         block
           integer :: i
           call me%vertices%set_vertex_id( [(i,i=1,nvertices)] )
         end block
       end associate

       block
         integer :: i
         do i = 1, vertices%length()
           maybe_vertex = vertices%get_element(i)
           associate(errors => maybe_vertex%errors())
             call assert(.not. errors%has_any(), "dag%from_json: .not. errors%has_any()", char(errors%to_string()))
           end associate
           select type (vertex_json => maybe_vertex%value_())
           type is (json_object_t)
             associate(dag_vertex => vertex_t(vertex_json))
               call me%vertices(i)%set_edges(dag_vertex%edges)
             end associate
           class default
             call assert(.false., "dag%from_json: vertex was not an object", char(vertex_json%to_compact_string()))
           end select
         end do
       end block
     class default
       call assert(.false., "dag%from_json: vertices was not an array", char(vertices%to_compact_string()))
     end select
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

  contains

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

  module procedure dag_save_digraph

    integer :: iunit, istat
    character(len=:),allocatable :: diagraph

    diagraph = generate_digraph(me, rankdir, dpi)

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat)

    if (istat==0) then
        write(iunit,fmt='(A)',iostat=istat) diagraph
    else
        write(*,*) 'error opening '//trim(filename)
    end if

    close(iunit,iostat=istat)
  contains

    function generate_digraph(me,rankdir,dpi) result(str)
      !! - Result is the string to write out to a *.dot file. (Called by dag_save_digraph())
      implicit none
      class(dag_t),intent(in)                :: me
      character(len=:),allocatable         :: str 
      character(len=*),intent(in),optional :: rankdir
        !! - Rank Direction which are applicable inputs to the -rankdir option on the digraph command
      integer,intent(in),optional          :: dpi 
        !! - dots per inch 

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

    end function generate_digraph

  end procedure

  elemental function integer_to_string(i) result(s)

    integer,intent(in) :: i
    integer, parameter :: max_number_width = 64
    character(len=max_number_width) :: s

    integer :: istat

    write(s,fmt='(ss,I0)',iostat=istat) i
    if (istat==0) then
        s = trim(adjustl(s))
    else
        s = '***'
    end if

  end function integer_to_string

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

  module procedure write_formatted

    type(json_object_t) :: me_json

    me_json = me%to_json()
    write(unit,*) char(me_json%to_expanded_string())

  end procedure

end submodule dag_implementation
