module clock_class
    implicit none
    private

    type, public :: a_clock
        private
        real :: current ! current time
        real :: total
        real :: step
        real :: lap
        real :: timer
    contains
        procedure :: init
        procedure :: next_step
        procedure :: time_is_not_over
        procedure :: timer_has_elapsed
        procedure :: get_step
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

    subroutine next_step( self )
        class(a_clock), intent(in out) :: self

        self%current = self%current + self%step
        self%timer   = self%timer   + self%step
    end subroutine next_step

    logical function time_is_not_over( self )
        class(a_clock), intent(in) :: self

        time_is_not_over = ( self%current <= self%total )
    end function time_is_not_over

    logical function timer_has_elapsed( self )
        class(a_clock), intent(in out) :: self

        timer_has_elapsed = ( self%timer >= self%lap )
        if (timer_has_elapsed) self%timer = self%timer - self%lap
    end function timer_has_elapsed

    real function get_step( self )
        class(a_clock), intent(in) :: self

        get_step = self%step
    end function get_step

end module clock_class