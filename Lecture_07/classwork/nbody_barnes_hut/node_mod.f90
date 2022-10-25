module node_mod
    use :: body_mod, only: a_body, body_acceleration_from

    type :: the_dims    ! cell dimensions
        real :: min(2)
        real :: max(2)
    end type the_dims

    type :: a_node_ptr
        type(a_node), pointer :: ptr => null()
    end type a_node_ptr

    type :: a_node      ! a quadtree node
        real :: M = 0.0
        real :: R(2) = [0.0, 0.0]
        type(the_dims) :: dims
        type(a_body),     pointer :: body => null()
        type(a_node_ptr), pointer :: subtree(:, :) => null()
    end type a_node

    integer, parameter :: node_is_empty  = 0
    integer, parameter :: node_is_a_body = 1
    integer, parameter :: node_is_a_cell = 2

contains

    integer function node_type( node )
        type(a_node_ptr), intent(in) :: node

        if (.not.associated( node%ptr )) then
            node_type = node_is_empty
        else if (.not.associated( node%ptr%subtree ) &
                 .and.associated( node%ptr%body )    ) then
            node_type = node_is_a_body
        else if (.not.associated( node%ptr%body )    &
                 .and.associated( node%ptr%subtree ) ) then
            node_type = node_is_a_cell
        else
            print "(a)", "node_mod::node_type: wrong node type, &
                &body and subtree pointers cannot have the same &
                &association status."
            stop
        end if
    end function node_type

    logical function node_is_far( node, body )
        type(a_node_ptr), intent(in) :: node
        type(a_body),     intent(in) :: body
        real, parameter :: theta = 0.5 ! opening angle
        real :: side, r

        side = node%ptr%dims%max(1) - node%ptr%dims%min(1)
        r    = norm2( node%ptr%R - body%r )
        node_is_far = ( side / r <= theta )
    end function node_is_far

    subroutine node_subdivide( node, r, quadrant, sub_dims )
        type(a_node),   intent(in)  :: node
        real,           intent(in)  :: r(2)
        integer,        intent(out) :: quadrant(2)
        type(the_dims), intent(out) :: sub_dims
        integer :: rank
        real    :: left, center, right

        do rank = 1, 2
            left   = node%dims%min(rank)
            right  = node%dims%max(rank)
            center = (left + right) / 2.0
            if (left <= r(rank) .and. r(rank) < center) then
                quadrant(rank)     = 1
                sub_dims%min(rank) = left
                sub_dims%max(rank) = center
            else if (center <= r(rank) .and. r(rank) < right) then
                quadrant(rank)     = 2
                sub_dims%min(rank) = center
                sub_dims%max(rank) = right
            else
                print "(a, i0, a, 3(es11.4, a))", &
                    "node_mod::node_subdivide: coordinate r(", rank, ") = ", &
                    r(rank), " is out of the cell range [", left, ", ", right, ")."
                stop
            end if
        end do
    end subroutine node_subdivide

    subroutine node_update_centroid( node, body )
        type(a_node), intent(in out) :: node
        type(a_body), intent(in)     :: body
        real :: RM(2) ! centroid mass momentum

        RM = node%R * node%M
        node%M = node%M + body%m
        node%R = (RM + body%r * body%m) / node%M
    end subroutine node_update_centroid

    recursive subroutine node_insert( node, body, dims )
        type(a_node_ptr),      intent(in out) :: node
        type(a_body), pointer, intent(in)     :: body
        type(the_dims),        intent(in)     :: dims
        integer        :: quadrant(2), i, j
        type(the_dims) :: sub_dims

        select case (node_type( node ))
        case (node_is_empty)
            allocate(node%ptr)
            node%ptr = a_node(M=body%m, R=body%r, dims=dims, body=body, subtree=null())
        case (node_is_a_body)
            associate( ptr => node%ptr )
                ! One more body turns this node into a cell.
                allocate(ptr%subtree(2, 2))
                do j = 1, 2
                    do i = 1, 2
                        ptr%subtree(i, j)%ptr => null()
                    end do
                end do

                ! Move the old body into some subcell.
                call node_subdivide( ptr, ptr%R, quadrant, sub_dims )
                associate( subcell => ptr%subtree(quadrant(1), quadrant(2)) )
                    call node_insert( subcell, ptr%body, sub_dims )
                end associate
                nullify(ptr%body)

                ! Insert the new body into some subcell.
                call node_update_centroid( ptr, body )
                call node_subdivide( ptr, body%r, quadrant, sub_dims )
                associate( subcell => ptr%subtree(quadrant(1), quadrant(2)) )
                    call node_insert( subcell, body, sub_dims )
                end associate
            end associate
        case (node_is_a_cell)
            associate( ptr => node%ptr )
                ! Add the new body into some subcell.
                call node_update_centroid( ptr, body )
                call node_subdivide( ptr, body%r, quadrant, sub_dims )
                associate( subcell => ptr%subtree(quadrant(1), quadrant(2)) )
                    call node_insert( subcell, body, sub_dims )
                end associate
            end associate
        case default
            stop "node_mod::node_insert: wrong node type."
        end select
    end subroutine node_insert

    recursive subroutine node_destroy( node )
        class(a_node_ptr), intent(inout) :: node

        select case (node_type( node ))
        case (node_is_empty)
            return
        case (node_is_a_body)
            deallocate(node%ptr)
        case (node_is_a_cell)
            block
                integer :: i, j
                do j = 1, 2
                    do i = 1, 2
                        call node_destroy( node%ptr%subtree(i, j) )
                    end do
                end do
            end block
            deallocate(node%ptr%subtree)
            deallocate(node%ptr)
        case default
            stop "node_mod::node_destroy: wrong node type"
        end select
    end subroutine node_destroy

    recursive function node_acceleration_from( node, body ) result(a_ij)
        real, dimension(2) :: a_ij
        type(a_node_ptr),      intent(in) :: node
        type(a_body), pointer, intent(in) :: body

        a_ij(:) = 0.0
        select case (node_type( node ))
        case (node_is_empty)
            return
        case (node_is_a_body)
            if (associated( body, node%ptr%body )) return
            a_ij(:) = body_acceleration_from( body, node%ptr%body )
        case (node_is_a_cell)
            if ( node_is_far( node, body ) ) then
                block
                    real, dimension(2) :: r_ij
                    r_ij(:) = node%ptr%R(:) - body%r(:)
                    a_ij(:) = ( node%ptr%M / norm2( r_ij )**3 ) * r_ij(:)
                end block
            else
                block
                    integer :: i, j
                    do j = 1, 2
                        do i = 1, 2
                            associate( subnode => node%ptr%subtree(i, j) )
                                a_ij(:) = a_ij(:) + node_acceleration_from( subnode, body )
                            end associate
                        end do
                    end do
                end block
            end if
        case default
            stop "node_mod::node_acceleration_from: wrong node type"
        end select
    end function node_acceleration_from

end module node_mod