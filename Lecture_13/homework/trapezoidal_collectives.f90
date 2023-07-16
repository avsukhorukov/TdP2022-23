! Trapezoidal rule integration with I/O.
!
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace
!   trapezoidal_collectives.f90
! $ mpirun -np 4 --oversubscribe ./a.out
!
program trapezoidal_collectives
    use mpi_f08
    use iso_fortran_env, only: real64
    implicit none
    integer :: my_rank, n_ranks
    real :: a, b, step, integral, full_integral
    integer :: m, ia, ib

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    if (my_rank == 0) then
        print '(a)', "Enter the interval [a, b] and the number of steps m (1.0e-3, 1.0, 1024):"
        read *, a, b, m
    end if
    call MPI_Bcast( a, 1, MPI_REAL,    0, MPI_COMM_WORLD )
    call MPI_Bcast( b, 1, MPI_REAL,    0, MPI_COMM_WORLD )
    call MPI_Bcast( m, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )

    ! There are m + 1 equidistant grid points with m intervals of a step in between.
    step = (b - a) / m

    call partition( id=my_rank, n_ids=n_ranks, size=m, l=ia, u=ib )

    integral = trap_integral( a + step * (ia - 1), a + step * ib, ib - ia + 1 )

    call MPI_Reduce( integral, full_integral, 1, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD )

    if (my_rank == 0) then
        print '(a, i0, 3(a, es12.6))', &
            "With ", m, " trapezoids the integral from ", a, " to ", b, " is ", full_integral
    end if

    call MPI_Finalize()
contains
    !---------------------------------------------------------------------------
    real function func( x )
        real, intent(in) :: x

        func = sin( 1.0 / x )
    end function func
    !---------------------------------------------------------------------------
    real function trap_integral( x_s, x_e, steps )
        real,    intent(in) :: x_s, x_e
        integer, intent(in) :: steps
        real    :: x, dx
        integer :: i

        dx = (x_e - x_s) / steps
        trap_integral = 0.5 * func( x_s ) + 0.5 * func( x_e )
        x = x_s ! i = 1
        do i = 2, steps
            x = x + dx
            trap_integral = trap_integral + func( x )
        end do
        trap_integral = trap_integral * dx
    end function trap_integral
    !---------------------------------------------------------------------------
    subroutine partition( id, n_ids, size, l, u )
        integer, intent(in)    :: id, n_ids, size
        integer, intent(inout) :: l, u
        integer :: remainder, quotient

        remainder = modulo( size, n_ids )
        quotient  = (size - remainder) / n_ids
        l = 1 + quotient * (id    ) + min( remainder, id     )
        u =     quotient * (id + 1) + min( remainder, id + 1 )
    end subroutine partition
    !---------------------------------------------------------------------------
end program trapezoidal_collectives
