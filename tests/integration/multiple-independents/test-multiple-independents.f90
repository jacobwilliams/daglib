program multiple_independents
  !!  Test topological sort and dependency matrix for a DAG with multiple independent nodes.
  use iso_fortran_env, only : error_unit
  use dag_interface, only : dag
  implicit none

  type(dag) :: d

  integer, allocatable :: order(:) !! topological sort
  logical, allocatable :: mat(:,:) !! dependency matrix

  integer :: i, row, istat
  integer,parameter :: n_nodes = 14, success=0
  logical :: expected_mat(n_nodes,n_nodes) = .false.
  integer, parameter :: expected_order(*) = [(i,i=1,n_nodes)]
  character(len=*),parameter :: filetype = 'pdf'  !! filetype for output plot ('pdf', png', etc.)
  character(len=*), parameter :: gray_square = 'shape=square,fillcolor="SlateGray1",style=filled'
  character(len=len(gray_square)), parameter :: silk_circle = 'shape=circle,fillcolor="cornsilk",style=filled'

  call d%set_vertices(n_nodes)
  call d%set_edges( 2,[integer::]) !2 depends on nothing
  call d%set_edges( 3,[integer::]) !3 depends on nothing
  call d%set_edges( 4,[3])  ; expected_mat(4,3)    = .true. !4 depends on  3
  call d%set_edges( 5,[4])  ; expected_mat(5,4)    = .true. !5 depends on  4
  call d%set_edges( 6,[3])  ; expected_mat(6,3)    = .true. !6 depends on  3
  call d%set_edges( 7,[4])  ; expected_mat(7,4)    = .true. !2 depends on  4
  call d%set_edges( 8,[5])  ; expected_mat(8,5)    = .true. !3 depends on  5
  call d%set_edges( 9,[6])  ; expected_mat(9,6)    = .true. !4 depends on  6
  call d%set_edges(10,[6,7]); expected_mat(10,6:7) = .true. !5 depends on  6 and 7
  call d%set_edges(11,[7,8]); expected_mat(11,7:8) = .true. !6 depends on  7 and 8
  call d%set_edges(12,[9])  ; expected_mat(12,9)   = .true. !2 depends on  9
  call d%set_edges(13,[10]) ; expected_mat(13,10)  = .true. !3 depends on 10
  call d%set_edges(14,[11]) ; expected_mat(14,11)  = .true. !4 depends on 11

  call d%toposort(order,istat)

  if (istat/=success) then
    write(error_unit, *) 'istat =', istat
    error stop
  end if

  if (any(order /= expected_order)) then
    write(error_unit, *) 'order =', order
    error stop
  end if

  do i = 1, n_nodes
    call d%set_vertex_info(i, attributes = merge(gray_square, silk_circle, any(i==[1,2,12,13,14])))
  end do
  call d%save_digraph('test2.dot','RL',300) ! TODO: verify internal generate_diagraph result against expected result
  call execute_command_line('dot -T'//filetype//' -o test2.'//filetype//' test2.dot')

  call d%generate_dependency_matrix(mat)

  if (any(mat .neqv. expected_mat)) then
    write(error_unit,*) 'main: incorrect dependency matrix:'
    do row = 1, n_nodes
      write(error_unit,*) merge('X', 'O', mat(row,:)) ! write row
    end do
    error stop
  end if

  sync all
  if (this_image()==1) print *,"Test passed."

end program multiple_independents
