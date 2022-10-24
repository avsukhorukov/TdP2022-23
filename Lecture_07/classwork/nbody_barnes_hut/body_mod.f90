module body_mod

    type :: a_body
        real :: m
        real :: r(2)
        real :: v(2)
        real :: a(2)
    end type a_body

contains

    subroutine body_init( body )
        type(a_body), intent(in out) :: body

        read *, body%m, body%r(:), body%v(:)
        body%a(:) = [0.0, 0.0]
    end subroutine body_init

    subroutine body_print_cooridinates( body )
        type(a_body), intent(in) :: body

        print "(2(es11.3, 1x))", body%r(:)
    end subroutine body_print_cooridinates

    subroutine body_half_kick( body, step )
        type(a_body), intent(in out) :: body
        real,         intent(in)     :: step

        body%v(:) = body%v(:) + (0.5 * step) * body%a(:)
    end subroutine body_half_kick

    subroutine body_drift( body, step )
        type(a_body), intent(in out) :: body
        real,         intent(in)     :: step

        body%r(:) = body%r(:) + step * body%v(:)
    end subroutine body_drift

    function body_acceleration_from( body, other_body ) result(a_ij)
        real, dimension(2) :: a_ij
        type(a_body), intent(in) :: body, other_body
        real, dimension(2) :: r_ij

        r_ij(:) = other_body%r(:) - body%r(:)
        a_ij(:) = ( other_body%m / norm2( r_ij(:) )**3 ) * r_ij(:)
    end function body_acceleration_from

end module body_mod
