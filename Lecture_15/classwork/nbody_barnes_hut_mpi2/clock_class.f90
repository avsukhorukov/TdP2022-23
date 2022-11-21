module clock_class
    use :: parallel_mod
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

        if (my_rank == root_rank) then
            read *, self%step
            read *, self%lap
            read *, self%total
        end if
        call MPI_Bcast( self%step,  1, MPI_REAL, root_rank, MPI_COMM_WORLD )
        call MPI_Bcast( self%lap,   1, MPI_REAL, root_rank, MPI_COMM_WORLD )
        call MPI_Bcast( self%total, 1, MPI_REAL, root_rank, MPI_COMM_WORLD )
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