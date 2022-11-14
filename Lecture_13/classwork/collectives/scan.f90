! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     scan.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
!
! Run with -np 5 and s = 17, use MPI_SUM as the operation.
program scan
    use :: mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root
    integer :: full_size, remainder, quotient, size, disp, b, e

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    root = 0

    if (my_rank == root) then
        print "(a)", "Enter the full size (17):"
        read *, full_size
    end if
    call MPI_Bcast( full_size, 1, MPI_INTEGER, root, MPI_COMM_WORLD )

    remainder = modulo( full_size, n_ranks )
    quotient  = (full_size - remainder) / n_ranks

    size = quotient + merge( 1, 0, my_rank < remainder )

    disp = 0
    call MPI_Exscan( size, disp, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD )

    b = disp + 1

    call MPI_Scan( size, e, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD )

    call print_vals()

    call MPI_Finalize()

contains

    subroutine print_vals()
        integer :: rnk

        do rnk = 0, n_ranks - 1
            if (rnk == my_rank) then
                print '(5(a, i2))', "Rank ", rnk, ": size=", size, ", disp=", disp, ", b:e=", b, ":", e
            end if
            call MPI_Barrier( MPI_COMM_WORLD )
        end do
    end subroutine print_vals

end program scan
