module bodies_mod
    use :: body_mod
    implicit none
    private :: bodies
    public :: bodies_init, bodies_destroy, bodies_get_size, &
        bodies_print_coordinates, bodies_half_kick, bodies_drift, &
        bodies_accelerate

    type(a_body), allocatable, dimension(:) :: bodies

contains

    subroutine bodies_init()
        integer :: n, i
        !
        read *, n
        allocate(bodies(n))
        do i = 1, n
            call body_init( bodies(i) )
        end do
    end subroutine bodies_init

    subroutine bodies_destroy()
        deallocate(bodies)
    end subroutine bodies_destroy

    integer function bodies_get_size() result(answer)
        answer = size( bodies )
    end function bodies_get_size

    subroutine bodies_print_coordinates()
        integer :: i
        !
        do i = 1, size( bodies )
            call body_print_coordinates( bodies(i) )
        end do
    end subroutine bodies_print_coordinates

    subroutine bodies_half_kick( step )
        real, intent(in) :: step
        integer :: i
        !
        do i = 1, size( bodies )
            call body_half_kick( bodies(i), step )
        end do
    end subroutine bodies_half_kick

    subroutine bodies_drift( step )
        real, intent(in) :: step
        integer :: i
        !
        do i = 1, size( bodies )
            call body_drift( bodies(i), step )
        end do
    end subroutine bodies_drift

    subroutine bodies_accelerate()
        integer :: i, j
        !
        do i = 1, size( bodies )
            associate( b_i => bodies(i) )
                b_i%a(:) = 0.0
                do j = 1, size( bodies )
                    if (j == i) cycle
                    b_i%a(:) = b_i%a(:) + body_acceleration_from( b_i, bodies(j) )
                end do
            end associate
        end do
    end subroutine bodies_accelerate

end module bodies_mod