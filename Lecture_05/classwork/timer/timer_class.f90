module timer_class
    use iso_fortran_env, only: int64, real64
    implicit none
    private

    type, public :: a_timer
        integer(kind=int64), private :: tick
        integer(kind=int64), private :: tack
        integer(kind=int64), private :: elapsed_ticks
        real(kind=real64),   private :: tick_rate
        logical,             private :: is_running = .false.
    contains
        procedure :: start => timer_start
        procedure :: stop  => timer_stop
        procedure :: show  => timer_elapsed
    end type a_timer

contains

    subroutine timer_start( self )
        class(a_timer), intent(in out) :: self

        if (self%is_running) then
            stop "a_timer%start(): it is running, you must call %stop first"
        else
            self%is_running = .true.
            call system_clock( count=self%tick, count_rate=self%tick_rate )
        end if
    end subroutine timer_start

    subroutine timer_stop( self )
        class(a_timer), intent(in out) :: self

        if (self%is_running) then
            self%is_running = .false.
            call system_clock( count=self%tack, count_rate=self%tick_rate )
            self%elapsed_ticks = self%tack - self%tick
        else
            stop "a_timer%stop(): is not running, you must call %start first"
        end if
    end subroutine timer_stop

    real function timer_elapsed( self ) result(seconds)
        class(a_timer), intent(in) :: self

        seconds = real( real( self%elapsed_ticks, kind=real64 ) / self%tick_rate )
    end function timer_elapsed

end module timer_class