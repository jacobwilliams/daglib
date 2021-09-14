submodule(dag_m) dag_s
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
  use iso_varying_string, only : varying_string, operator(//), char, get, put, var_str
  use intrinsic_array_m, only : intrinsic_array_t

  implicit none

  type vertex_status_t
    logical :: marked=.false., checking=.false.
  end type

contains

  module function toposort(vertices) result(order)
    !! Provide array of vertex numbers ordered in a way that respects dependencies
    type(vertex_t), intent(in) :: vertices(:)
    integer, allocatable :: order(:), discovered(:), searched(:)
    integer v

    call assert(all([(allocated(vertices(v)%edges), v=1, size(vertices))]), &
        "dag_s toposort: (all([(allocated(vertices(v)%edges), v=1, size(vertices))])")

    allocate(order(0), discovered(0), searched(0))
   
    block
      integer, allocatable :: previously_found(:)

      allocate(previously_found(0))

      do v = 1, size(vertices)
        if (.not. any(v == searched)) then
          call depth_first_search(v, previously_found, searched, order)
          previously_found = [previously_found, searched]
        end if
      end do
    end block

  contains

    recursive subroutine depth_first_search(v, d, s, o)
      integer, intent(in) :: v, d(:)
      integer, intent(out), allocatable :: s(:)
      integer, intent(inout), allocatable :: o(:)
      integer, allocatable :: dependencies(:), s_local(:), p_local(:)
      integer w

      call assert(any(v == d), "depth_first_search: cycle detected", intrinsic_array_t([v,d]))
        
      dependencies = vertices(v)%get_dependencies()

      o = [o, v]
      allocate(s_local(0), p_local(0))

      do w = 1, size(dependencies)
        if (.not. any(dependencies(w) == s_local)) then
          call depth_first_search(dependencies(w), [d, dependencies(w)], s_local, o)
          p_local = [p_local, s_local]
          s_local = p_local
        end if
      end do

      s = [v, searched]
    end subroutine

  end function toposort
                                   
  module procedure is_sorted_and_acyclic
    
    if (.not. allocated(self%order)) then
      is_sorted_and_acyclic = .false.
      return
    end if
    
    associate(num_vertices => size(self%vertices), order_size => size(self%order))
      call assert(order_size == num_vertices, "dag_t%is_sorted_and_acyclic: size(self%vertices) == size(self%order)", &
        intrinsic_array_t([order_size, num_vertices]))
    
      block
        integer i
    
        associate(vertices_sorted => [( self%vertices(self%order(i))%get_edges() < i , i=1, num_vertices)])
          is_sorted_and_acyclic = all(vertices_sorted)
        end associate
      end block
    
    end associate
    
  end procedure

  module procedure construct_from_json
    type(fallible_json_value_t) :: maybe_vertices

    associate(errors => maybe_vertices%errors())
      call assert(.not. errors%has_any(), "dag_s construct_from_json: .not. errors%has_any()", char(errors%to_string()))
    end associate

    select type (vertices => maybe_vertices%value_())
    type is (json_array_t)

      associate(nvertices => vertices%length())
        allocate(dag%vertices(nvertices))
        block
          integer i
          call dag%vertices%set_vertex_id( [(i,i=1,nvertices)] )
        end block
      end associate

      block
        integer i
        do i = 1, vertices%length()
          associate(maybe_vertex => vertices%get_element(i))
            associate(errors => maybe_vertex%errors())
              call assert(.not. errors%has_any(), "dag_s construct_from_json: .not. errors%has_any()", char(errors%to_string()))
            end associate

            select type (vertex_json => maybe_vertex%value_())
            class default
              call assert(.false., "dag_s construct_from_json: vertex was not an object", char(vertex_json%to_compact_string()))
            type is (json_object_t)
              associate(dag_vertex => vertex_t(vertex_json))
                call dag%vertices(i)%set_edges(dag_vertex%edges)
              end associate
            end select
          end associate
        end do
      end block

    class default
      call assert(.false., "dag%from_json: vertices was not an array", char(vertices%to_compact_string()))
    end select
  end procedure

   module procedure to_json
     type(fallible_json_string_t) maybe_key
     type(error_list_t) errors

     maybe_key = fallible_json_string_t("vertices")
     errors = maybe_key%errors()
     call assert(.not. errors%has_any(), "dag%to_json: .not. errors%has_any()", char(errors%to_string()))

     block
       type(json_array_t) vertices_value

       vertices_value = json_array_t(json_element_t(self%vertices%to_json())) 

       associate(vertices_key => maybe_key%string())
         json_object = json_object_t([vertices_key], [json_element_t(vertices_value)])
       end associate
     end block
   end procedure

   module procedure construct_from_components
     dag%vertices = vertices
     dag%order = toposort(dag%vertices)
   end procedure


  module procedure dependency_matrix


    associate(num_vertices => size(self%vertices))
      if (num_vertices > 0) then

        allocate(mat(num_vertices,num_vertices))
        mat = .false.

        block
        integer i
          do concurrent(i=1:num_vertices)
            if (allocated(self%vertices(i)%edges)) mat(i,self%vertices(i)%edges) = .true.
          end do
        end block

      end if
    end associate

  end procedure

  module procedure num_vertices
    num_vertices = size(self%vertices)
  end procedure

  module procedure dependencies_for
    dependency_ids = self%vertices(vertex_id)%get_edges()
  end procedure

  module procedure save_digraph

    integer :: iunit, istat
    character(len=:),allocatable :: diagraph

    diagraph = generate_digraph(self, rankdir, dpi)

    open(newunit=iunit,file=filename,status='REPLACE',iostat=istat)

    if (istat==0) then
        write(iunit,fmt='(A)',iostat=istat) diagraph
    else
        write(*,*) 'error opening '//trim(filename)
    end if

    close(iunit,iostat=istat)
  contains

    function generate_digraph(self,rankdir,dpi) result(str)
      !! - Result is the string to write out to a *.dot file. (Called by save_digraph())
      implicit none
      class(dag_t),intent(in)                :: self
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
      do i=1,size(self%vertices)
        label = 'label="'//trim(adjustl(self%vertices(i)%get_label()))//'"'
        attributes = '['//trim(adjustl(self%vertices(i)%get_attributes()))//','//label//']'
        str = str//tab//integer_to_string(i)//' '//attributes//newline
        if (i==size(self%vertices)) str = str//newline
      end do

      ! define the dependencies:
      do i=1,size(self%vertices)
          if (allocated(self%vertices(i)%edges)) then
              n_edges = size(self%vertices(i)%edges)
              str = str//tab//integer_to_string(i)//merge(' -> ','    ',n_edges/=0)
              do j=1,n_edges
                  ! comma-separated list:
                  str = str//integer_to_string(self%vertices(i)%edges(j))
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
    type(dag_t) :: self_local
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
      self_local = construct_from_json(object)
      self%vertices = self_local%vertices
    class default
      call assert(.false., "dag%read_formatted: didn't get a json object")
    end select

  end procedure

  module procedure write_formatted

    type(json_object_t) :: me_json

    me_json = self%to_json()
    write(unit,*) char(me_json%to_expanded_string())

  end procedure

end submodule dag_s
