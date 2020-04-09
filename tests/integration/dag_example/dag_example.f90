!*******************************************************************************
!>
!  DAG module test program.

    program dag_example

    use iso_fortran_env, only : error_unit
    use dag_module, only : dag

    implicit none

    type(dag) :: d

    integer, allocatable :: order(:) !! topological sort
    logical, allocatable :: mat(:,:) !! dependency matrix

    integer, parameter :: expected_order(*) = [1,2,5,3,4]
    logical, parameter :: expected_mat(*,*) = reshape( [ &
      .false., .true. , .true. , .false., .false., .false., &
      .false., .false., .false., .false., .true. , .true. , &
      .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .true. , &
      .false., .false., .true. , .true. , .false., .false., &
      .false., .false., .false., .false., .false., .false. &
      ], [6,6])
      !! expected dependency matrix
    integer,parameter :: n_nodes = 6, success=0
    character(len=*), parameter :: filetype = 'pdf'  !! filetype for output plot ('pdf', png', etc.)
    character(len=*), parameter :: gray_square = 'shape=square,fillcolor="SlateGray1",style=filled'
    character(len=len(gray_square)), parameter :: silk_circle = 'shape=circle,fillcolor="cornsilk",style=filled'

    integer :: istat
    integer :: i, row

    call d%set_vertices(n_nodes)
    call d%set_edges(2,[1])     !2 depends on 1
    call d%set_edges(3,[5,1])   !3 depends on 5 and 1
    call d%set_edges(4,[5])     !4 depends on 5
    call d%set_edges(5,[2])     !5 depends on 2
    call d%set_edges(6,[2,4])   !6 depends on 2 and 4

    call d%toposort(order,istat)

    if (istat /= success) then
      write(error_unit, *) 'istat =', istat
      error stop
    end if

    if (any(order /= expected_order)) then
      write(error_unit, *) 'order =', order
      error stop
    end if

    do i = 1, n_nodes
      call d%set_vertex_info(i, attributes = merge(gray_square, silk_circle, any(i==[3,6])))
    end do
    call d%save_digraph('test.dot','RL',300) ! TODO: verify internal generate_diagraph result against expected result
    call execute_command_line('dot -T'//filetype//' -o test.'//filetype//' test.dot')

    call d%generate_dependency_matrix(mat)
    if (any(mat .neqv. expected_mat)) then
      write(error_unit,*) 'main: incorrect dependency matrix:'
      do row = 1, n_nodes
        write(error_unit,*) merge('X', 'O', mat(row,:)) ! write column
      end do
      error stop
    end if

    ! cleanup:
    call d%destroy()

    print *,"Test passed."
    end program dag_example
!*******************************************************************************
