module tree_mod
    use :: parallel_mod
    use :: node_mod, only : a_node_ptr, the_dims, node_insert, &
           node_acceleration_from, node_destroy, node_belongs
    use :: body_mod, only : a_body
    use :: bodies_mod, only : bodies, bodies_get_dimensions
    implicit none

    type(a_node_ptr) :: root

contains

    subroutine tree_create()
        type(the_dims) :: dims
        type(a_body), pointer :: body
        integer :: i

        dims = bodies_get_dimensions()
        do i = 1, size( bodies )
            body => bodies(i)
            if (.not.node_belongs( body%r, dims )) cycle
            call node_insert( root, body, dims )
        end do
    end subroutine tree_create

    subroutine tree_accelerate()
        integer :: i

        do i = 1, size( bodies )
            associate( b_i => bodies(i) )
                b_i%a(:) = node_acceleration_from( root, b_i )
                call MPI_Allreduce( MPI_IN_PLACE, b_i%a, 2, MPI_REAL, MPI_SUM, MPI_COMM_WORLD )
            end associate
        end do
        ! call MPI_Allreduce( MPI_IN_PLACE, bodies, size( bodies ), &
        !                    a_body_datatype, sum_body_a_op, MPI_COMM_WORLD )
    end subroutine tree_accelerate

    subroutine tree_destroy()
        call node_destroy( root )
    end subroutine tree_destroy
end module tree_mod