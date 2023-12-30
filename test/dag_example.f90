!*******************************************************************************
!>
!  DAG module test program.

    program dag_example

    use dag_module, ip => daglib_ip

    implicit none

    type(dag) :: d
    integer(ip),dimension(:),allocatable :: order
    integer(ip) :: istat
    integer(ip) :: i,r,c
    logical,dimension(:,:),allocatable :: mat !! dependency matrix

    integer(ip),parameter :: n_nodes = 7
    character(len=*),parameter :: filetype = 'pdf'  !! filetype for output plot ('pdf', png', etc.)

    ! TODO combine set_edges and set_vertex_info into one routine maybe.

    call d%set_vertices(n_nodes)
    call d%set_edges(2_ip,[1_ip])        !2 depends on 1
    call d%set_edges(3_ip,[5_ip,1_ip])   !3 depends on 5 and 1
    call d%set_edges(4_ip,[5_ip])        !4 depends on 5
    call d%set_edges(5_ip,[2_ip])        !5 depends on 2
    call d%set_edges(6_ip,[2_ip,4_ip])   !6 depends on 2 and 4
    ! note that node 7 isn't connected to any other node

    call d%toposort(order,istat)

    write(*,*) ''
    write(*,*) 'istat=', istat
    write(*,*) 'order=', order ! prints 1,2,5,3,4

    do i = 1, n_nodes
        if (i==3_ip .or. i==6_ip) then
            call d%set_vertex_info(i,attributes='shape=square,fillcolor="SlateGray1",style=filled')
        else
            call d%set_vertex_info(i,attributes='shape=circle,fillcolor="cornsilk",style=filled')
        end if
    end do

    write(*,*) ''
    write(*,*) 'diagraph:'
    write(*,*) ''
    call save_plot('test1')

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

    ! test removing a node:
    call d%remove_vertex(5_ip)
    call save_plot('test1_5-removed')

    ! test removing an edge:
    call d%remove_edge(5_ip,4_ip) ! the orignal node 6 is now 5
    call save_plot('test1_node-5-removed_6-4-edge-removed')

    ! test adding an edge:
    call d%add_edge(ivertex=5_ip,iedge=1_ip, &
                    label='added',&
                    attributes='penwidth=2,arrowhead=none,color=red')
    call save_plot('test1_node-5-removed_6-4-edge-removed-edge-added')

    ! cleanup:
    call d%destroy()

    contains

        subroutine save_plot(filename)
            character(len=*),intent(in) :: filename
            call d%save_digraph(filename//'.dot','RL',300_ip)
            call execute_command_line('cat '//filename//'.dot')
            call execute_command_line('dot -T'//filetype//' -o '//&
                                        filename//'.'//filetype//' '//&
                                        filename//'.dot')
        end subroutine save_plot

    end program dag_example
!*******************************************************************************
