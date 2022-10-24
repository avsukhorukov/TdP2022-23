module node_mod
    use :: body_mod

    type :: the_dims    ! dimensions of a cell
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

    ! TODO
    recursive subroutine node_insert( node, body, dims )
        type(a_node_ptr),      intent(inout) :: node
        type(a_body), pointer, intent(in)    :: body
        type(the_dims),        intent(in)    :: dims
        ! ...
    end subroutine node_insert

end module node_mod