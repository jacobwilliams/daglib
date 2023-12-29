!*******************************************************************************
!>
!  DAG module test program.

    program dag_example_2

    use dag_module

    implicit none

    type(dag) :: d
    integer,dimension(:),allocatable :: order
    integer :: istat
    integer :: i,r,c
    logical,dimension(:,:),allocatable :: mat !! dependency matrix

    integer,parameter :: n_nodes = 12
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

    call d%set_edges( 1  , [2,5,10 ] )    !  [penwidth=1, arrowhead=none];
    call d%set_edges( 3  , [4,5,6,1] )    !  [penwidth=1, arrowhead=none]
    call d%set_edges( 4  , [11,6   ] )    !  [penwidth=1, arrowhead=none]
    call d%set_edges( 6  , [7      ] )    !  [penwidth=1, arrowhead=none]
    call d%set_edges( 7  , [11,4   ] )    !  [penwidth=1, arrowhead=none]
    call d%set_edges( 8  , [9,5    ] )    !  [penwidth=1, arrowhead=none]
    call d%set_edges( 9  , [1,3    ] )    !  [penwidth=1, arrowhead=none]
    call d%set_edges( 10 , [8,6,9  ] )    !  [penwidth=1, arrowhead=none]
    call d%set_edges( 11 , [1,6,9,3] )    !  [penwidth=1, arrowhead=none]
    call d%set_edges( 12 , [10     ] )    !  [penwidth=1, arrowhead=none]

    ! [penwidth=1, arrowhead=none]

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

    call d%save_digraph('test2.dot','RL',300)
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
