! Factorial, calculated in a 1D ring topology.
!
! Instead of building a ring topology, use a parallel prefix reduction (scan).
! 
! Compile and run:
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace ring_scan.f90
! $ mpirun -np 6 --oversubscribe ./a.out
program ring_scan
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks
    integer :: i, factorial
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    if (my_rank == 0) then
        read *, i
    else
        i = my_rank + 1
    end if

    call MPI_Scan( i, factorial, 1, MPI_INTEGER, MPI_PROD, MPI_COMM_WORLD )

    if (my_rank == n_ranks - 1) then
        call MPI_Send( factorial, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD )
    else if (my_rank == 0) then
        call MPI_Recv( factorial, 1, MPI_INTEGER, n_ranks - 1, 0, MPI_COMM_WORLD, status )
        print *, factorial
    end if

    call MPI_Finalize()

end program ring_scan
