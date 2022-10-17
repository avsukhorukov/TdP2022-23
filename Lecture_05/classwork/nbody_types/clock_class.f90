module clock_class
    implicit none
    private

    type, public :: a_clock
        real, private :: current ! current time
        real, private :: total   ! total integration time
        real, private :: step    ! integration step
        real, private :: timer   ! cyclic time count for output
        real, private :: lap     ! time cycle to output coordinates
    contains
        procedure, public :: init
        procedure, public :: next_step
        procedure, public :: timer_has_elapsed
        procedure, public :: time_is_not_over
        procedure, public :: get_step
    end type a_clock

contains

    subroutine init( self )
        class(a_clock), intent(in out) :: self

        read *, self%step
        read *, self%lap
        read *, self%total
        self%current = 0.0
        self%timer   = 0.0
    end subroutine init

    real function get_step( self ) result(step)
        class(a_clock), intent(in) :: self

        step = self%step
    end function get_step

    subroutine next_step( self )
        class(a_clock), intent(in out) :: self

        self%current = self%current + self%step
        self%timer   = self%timer   + self%step
    end subroutine next_step

    logical function time_is_not_over( self )
        class(a_clock), intent(in) :: self

        time_is_not_over = ( self%current <= self%total )
    end function time_is_not_over

    logical function timer_has_elapsed( self ) result(has_elapsed)
        class(a_clock), intent(in out) :: self

        has_elapsed = ( self%timer >= self%lap )
        if (has_elapsed) self%timer = self%timer - self%lap
    end function timer_has_elapsed

end module clock_class