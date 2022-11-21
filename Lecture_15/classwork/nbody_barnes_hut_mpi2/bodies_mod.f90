module bodies_mod
    use :: body_mod
    use :: node_mod, only: the_dims
    implicit none

    type(a_body), allocatable, dimension(:), target :: bodies

contains

    subroutine bodies_init()
        integer :: n, i
        !
        if (my_rank == root_rank) read *, n
        call MPI_Bcast( n, 1, MPI_INTEGER, root_rank, MPI_COMM_WORLD )
        allocate(bodies(n))
        do i = 1, size( bodies )
            call body_init( bodies(i) )
        end do
        call body_create_datatypes()
        call MPI_Bcast( bodies, n, a_body_datatype, root_rank, MPI_COMM_WORLD )
    end subroutine bodies_init

    subroutine bodies_destroy()
        deallocate(bodies)
        call body_free_datatypes()
    end subroutine bodies_destroy

    subroutine bodies_print_cooridinates()
        integer :: i

        do i = 1, size( bodies )
            call body_print_cooridinates( bodies(i) )
        end do
    end subroutine bodies_print_cooridinates

    subroutine bodies_half_kick( step )
        real, intent(in) :: step
        integer :: i

        do i = 1, size( bodies )
            call body_half_kick( bodies(i), step )
        end do
    end subroutine bodies_half_kick

    subroutine bodies_drift( step )
        real, intent(in) :: step
        integer :: i

        do i = 1, size( bodies )
            call body_drift( bodies(i), step )
        end do
    end subroutine bodies_drift

    ! subroutine bodies_accelerate()
    !     integer :: i, j
    
    !     do i = 1, size( bodies )
    !         associate( b_i => bodies(i) )
    !             b_i%a(:) = 0.0
    !             do j = 1, size( bodies )
    !                 if (j == i) cycle
    !                 b_i%a(:) = b_i%a(:) + body_acceleration_from( b_i, bodies(j) )
    !             end do
    !         end associate
    !     end do
    ! end subroutine bodies_accelerate

    integer function bodies_get_size()
        bodies_get_size = size( bodies )
    end function bodies_get_size

    ! Get dimensions of the square bounding box that circumscribes all the
    ! bodies.  The global ranges are decomposed into four quadrants at the first
    ! level of the tree.  The topology array defines the indices [r, c] =
    ! [0..1, 0..1] which can be used to quickly find the dimensions of this
    ! decomposition.
    function bodies_get_dimensions() result(answer)
        type(the_dims) :: answer
        real, dimension(2) :: mins, maxs, centers
        real :: span
        integer :: i

        mins =  huge( 0.0 )
        maxs = -huge( 0.0 )
        do i = 1, size( bodies )
            mins = min( mins, bodies(i)%r )
            maxs = max( maxs, bodies(i)%r )
        end do
        span = maxval(maxs - mins) * 1.001 ! +0.1% to cover the border
        centers = (mins + maxs) / 2.0
        answer%min = centers - ( span / 2.0 ) * (topology - 1)
        answer%max = centers + ( span / 2.0 ) * (topology)
    end function bodies_get_dimensions

end module bodies_mod