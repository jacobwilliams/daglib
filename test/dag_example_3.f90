!*******************************************************************************
!>
!  DAG module test program: Advent of Code 2023 Day 25 (test problem).

    program dag_example_3

    use dag_module, ip => daglib_ip

    implicit none

    type(dag) :: d
    integer(ip) :: i, n_nodes
    character(len=3),dimension(:),allocatable :: labels

    character(len=*),parameter :: filetype = 'pdf'  !! filetype for output plot ('pdf', png', etc.)

    n_nodes = 0
    !allocate(labels(0))
    do i = 1, 2
        ! first pass just gets the nodes, 2nd gets the dependencies
        call process(i, 'jqt', ['rhn', 'xhk', 'nvd'])
        call process(i, 'rsh', ['frs', 'pzl', 'lsr'])
        call process(i, 'xhk', ['hfx'])
        call process(i, 'cmg', ['qnr', 'nvd', 'lhk', 'bvb'])
        call process(i, 'rhn', ['xhk', 'bvb', 'hfx'])
        call process(i, 'bvb', ['xhk', 'hfx'])
        call process(i, 'pzl', ['lsr', 'hfx', 'nvd'])
        call process(i, 'qnr', ['nvd'])
        call process(i, 'ntq', ['jqt', 'hfx', 'bvb', 'xhk'])
        call process(i, 'nvd', ['lhk'])
        call process(i, 'lsr', ['lhk'])
        call process(i, 'rzs', ['qnr', 'cmg', 'lsr', 'rsh'])
        call process(i, 'frs', ['qnr', 'lhk', 'lsr'])
        call process(i, 'hfx')
        call process(i, 'lhk')
        if (i==1) then
            write(*,*) 'set_vertices !'
            call d%set_vertices(n_nodes, labels=labels)
        end if
    end do

    call d%save_digraph('test3.dot','RL',300_ip)
    call execute_command_line('cat test3.dot')
    call execute_command_line('dot -T'//filetype//' -o test3.'//filetype//' test3.dot')

    contains
        subroutine process(icase, node, dependson)
            integer(ip),intent(in) :: icase
            character(len=3),intent(in) :: node
            character(len=3),dimension(:),intent(in),optional :: dependson
            character(len=100),dimension(:),allocatable :: edge_attributes
            integer(ip) :: i !! counter
            integer(ip),dimension(1) :: idx
            character(len=*),parameter :: DEFAULT_EDGE = 'arrowhead=none'
            character(len=*),parameter :: EDGES_TO_CUT = 'penwidth=2,arrowhead=none,color=red'

            if (icase==1) then
                n_nodes = n_nodes + 1
                if (allocated(labels)) then
                    labels = [labels, node]
                else
                    labels = [node]
                end if
            else
                if (present(dependson)) then
                    allocate(edge_attributes(size(dependson)))
                    edge_attributes = DEFAULT_EDGE
                    if (node=='pzl' .and. any(findloc(labels,'hfx')>0)) then
                        idx = findloc(dependson,'hfx'); i = idx(1)
                        edge_attributes(i) = EDGES_TO_CUT
                        call d%set_edges(node_index(node), node_index(dependson), attributes = edge_attributes)
                    else if (node=='cmg' .and. any(findloc(labels,'bvb')>0)) then
                        idx = findloc(dependson,'bvb'); i = idx(1)
                        edge_attributes(i) = EDGES_TO_CUT
                        call d%set_edges(node_index(node), node_index(dependson), attributes = edge_attributes)
                    else if (node=='jqt' .and. any(findloc(labels,'nvd')>0)) then
                        idx = findloc(dependson,'nvd'); i = idx(1)
                        edge_attributes(i) = EDGES_TO_CUT
                        call d%set_edges(node_index(node), node_index(dependson), attributes = edge_attributes)
                    else
                        call d%set_edges(node_index(node), node_index(dependson), attributes = edge_attributes)
                    end if
                end if
            end if
        end subroutine process

        pure elemental integer(ip) function node_index(node)
            !! find the node number for this name
            character(len=3),intent(in) :: node
            integer(ip),dimension(1) :: idx
            idx = findloc(labels,node)
            node_index = idx(1)
        end function node_index

    end program dag_example_3
!*******************************************************************************