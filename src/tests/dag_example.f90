!*******************************************************************************
!>
!  DAG module test program.

    program dag_example

    use dag_module

    implicit none

    type(dag) :: d
    integer,dimension(:),allocatable :: order
    integer :: istat
    integer :: i,r,c
    logical,dimension(:,:),allocatable :: mat !! dependency matrix

    integer,parameter :: n_nodes = 6
    character(len=*),parameter :: filetype = 'pdf'  !! filetype for output plot ('pdf', png', etc.)

    ! TODO combine set_edges and set_vertex_info into one routine maybe.

    call d%set_vertices(n_nodes)
    call d%set_edges(2,[1])     !2 depends on 1
    call d%set_edges(3,[5,1])   !3 depends on 5 and 1
    call d%set_edges(4,[5])     !4 depends on 5
    call d%set_edges(5,[2])     !5 depends on 2
    call d%set_edges(6,[2,4])   !6 depends on 2 and 4

    call d%toposort(order,istat)

    write(*,*) ''
    write(*,*) 'istat=', istat
    write(*,*) 'order=', order ! prints 1,2,5,3,4

    do i = 1, n_nodes
        if (i==3 .or. i==6) then
            call d%set_vertex_info(i,attributes='shape=square,fillcolor="SlateGray1",style=filled')
        else
            call d%set_vertex_info(i,attributes='shape=circle,fillcolor="cornsilk",style=filled')
        end if
    end do

    write(*,*) ''
    write(*,*) 'diagraph:'
    write(*,*) ''

    call d%save_digraph('test.dot','RL',300)
    call execute_command_line('cat test.dot')
    call execute_command_line('dot -T'//filetype//' -o test.'//filetype//' test.dot')

    write(*,*) ''
    write(*,*) 'dependency matrix:'
    write(*,*) ''
    call d%generate_dependency_matrix(mat)
    do r=1,n_nodes
        do c=1,n_nodes
            if (mat(r,c)) then
                write(*,'(A)',advance='NO') 'X'
            else
                write(*,'(A)',advance='NO') 'O'
            end if
        end do
            write(*,'(A)') ''
    end do

    ! cleanup:
    call d%destroy()

    end program dag_example
!*******************************************************************************
