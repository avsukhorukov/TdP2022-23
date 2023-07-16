! Trapezoidal rule integration with I/O.
!
! Compile and run:
!
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     trapezoidal_p2p.f90
! $ mpirun -np 4 --oversubscribe ./a.out
!
program trapezoidal_p2p
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, r, root
    real :: a, b, step, integral
    integer :: m, ia, ib
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    root = 0

    if (my_rank == root) then
        print '(a)', "Enter the interval [a, b] and the number of steps m (1.0e-3, 1.0, 1024):"
        read *, a, b, m
        do r = 1, n_ranks - 1
            call MPI_Send( a, 1, MPI_REAL,    r, 0, MPI_COMM_WORLD )
            call MPI_Send( b, 1, MPI_REAL,    r, 0, MPI_COMM_WORLD )
            call MPI_Send( m, 1, MPI_INTEGER, r, 0, MPI_COMM_WORLD )
        end do
    else ! (my_rank /= root)
        call MPI_Recv( a, 1, MPI_REAL,    root, 0, MPI_COMM_WORLD, status )
        call MPI_Recv( b, 1, MPI_REAL,    root, 0, MPI_COMM_WORLD, status )
        call MPI_Recv( m, 1, MPI_INTEGER, root, 0, MPI_COMM_WORLD, status )
    end if

    ! There are m + 1 equidistant grid points with m intervals of a step in between.
    step = (b - a) / m

    call partition( my_rank, n_ranks, m, ia, ib )

    integral = trap_integral( a + step * (ia - 1), a + step * ib, ib - ia + 1 )

    if (my_rank /= root) then
        call MPI_Send( integral, 1, MPI_REAL, root, 0, MPI_COMM_WORLD )
    else ! (my_rank == root)
        block
            real :: full_integral

            full_integral = integral
            do r = 1, n_ranks - 1
                call MPI_Recv( integral, 1, MPI_REAL, r, 0, MPI_COMM_WORLD, status )
                full_integral = full_integral + integral
            end do
            print '(a, i0, 3(a, es12.6))', &
                "With ", m, " trapezoids the integral from ", a, " to ", b, " is ", full_integral
        end block
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
        x = x_s
        do i = 2, steps
            !x = x + dx
            x = x_s + dx * (i - 1)
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

end program trapezoidal_p2p
