! Write a program that distributes an identity matrix over the processes.  The
! dimension and the number of processes are given.  Distribute by rows (Fortran).
! The program reads the matrix size S, takes the number of processes N, and
! calculates how many columns (with remainder) must take every process.
! You can either operate with local or global indices on each process.  At the
! end, rank 0 collects parts of the matrix from other ranks and prints the
! entire thing.
!
! Compile and run:
!
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!   parallel_mod.f90 matrix_by_rows_p2p.F90
! $ mpirun -np 3 --oversubscribe ./a.out # enter 10
!
program matrix_by_rows_p2p
    use :: mpi_f08
    use :: parallel_mod, only :
    implicit none
    integer              :: s       ! matrix size
    integer, allocatable :: e(:, :) ! matrix itself
    integer :: my_rank, n_ranks

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    if (my_rank == 0) then
        print "(a)", "Enter the matrix size S:"
        read *, s
        !...
    else
        !...
    end if

    call MPI_Finalize()
end program matrix_by_rows_p2p
