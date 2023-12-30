!*******************************************************************************
!>
!  DAG module test program.

    program dag_example_2

    use dag_module, ip => daglib_ip

    implicit none

    type(dag) :: d
    integer(ip) :: i,r,c
    logical,dimension(:,:),allocatable :: mat !! dependency matrix

    integer(ip),parameter :: n_nodes = 12
    character(len=*),parameter :: filetype = 'pdf'  !! filetype for output plot ('pdf', png', etc.)
    character(len=*),dimension(n_nodes),parameter :: labels = ['yb   ', &
                                                               'pi   ', &
                                                               'jg   ', &
                                                               'ej   ', &
                                                               'KN   ', &
                                                               'LD   ', &
                                                               'start', &
                                                               'end  ', &
                                                               'UF   ', &
                                                               'xd   ', &
                                                               'qx   ', &
                                                               'DM   ']

    call d%set_vertices(n_nodes, labels=labels)

    call d%set_edges( 1_ip  , [2_ip,5_ip,10_ip    ] )
    call d%set_edges( 3_ip  , [4_ip,5_ip,6_ip,1_ip] )
    call d%set_edges( 4_ip  , [11_ip,6_ip         ] )
    call d%set_edges( 6_ip  , [7_ip               ] )
    call d%set_edges( 7_ip  , [11_ip,4_ip         ] )
    call d%set_edges( 8_ip  , [9_ip,5_ip          ] )
    call d%set_edges( 9_ip  , [1_ip,3_ip          ] )
    call d%set_edges( 10_ip , [8_ip,6_ip,9_ip     ] )
    call d%set_edges( 11_ip , [1_ip,6_ip,9_ip,3_ip] )
    call d%set_edges( 12_ip , [10_ip              ], &
                        label=['hello'], &
                        attributes=['penwidth=2,arrowhead=none,color=red,fontcolor=red'] )

    do i = 1, n_nodes
        if (i==7 .or. i==8) then
            call d%set_vertex_info(i,label=labels(i), attributes="shape=square,fillcolor=SlateGray1,style=filled")
        else
            call d%set_vertex_info(i,attributes="shape=circle,fillcolor=cornsilk,style=filled")
        end if
    end do

    write(*,*) ''
    write(*,*) 'diagraph:'
    write(*,*) ''

    call d%save_digraph('test2.dot','RL',300_ip)
    call execute_command_line('cat test2.dot')
    call execute_command_line('dot -T'//filetype//' -o test2.'//filetype//' test2.dot')

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

    end program dag_example_2
!*******************************************************************************
