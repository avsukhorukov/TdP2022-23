module tree_mod
    use :: node_mod
    use :: bodies_mod

    type(a_node_ptr) :: tree

contains

    subroutine tree_create()
        type(the_dims) :: dims
        type(a_body), pointer :: body
        integer :: i

        dims = bodies_get_dimensions()
        do i = 1, size( bodies )
            body => bodies(i)
            call node_insert( tree, body, dims )
        end do
    end subroutine tree_create


end module tree_mod