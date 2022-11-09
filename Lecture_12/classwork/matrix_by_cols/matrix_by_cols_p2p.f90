! Matrix distribution by columns.
!
! Distribute a global N x N matrix over P processes so that each process has a
! local portion of it in its memory.  Initialize such a portion with the process
! rank.  N is not necessarily divisible by P.
! Each process sends its first and last columns (in F) to neighbors: the fist to
! the left rank, the last to the right rank.  Each process must allocate extra
! ghost cells with two columns.
!
! Compile and run:
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace
!   parallel_mod.f90 matrix_by_cols_p2p.f90
! $ mpirun -np 3 --oversubscribe ./a.out
!
! Enter size 10.
program matrix_by_cols_p2p
    use :: mpi_f08
    use :: parallel_mod, only :
    implicit none
    integer, parameter   :: n = 10
    integer, allocatable :: matrix(:, :)
    integer :: my_rank, n_ranks

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    ! ...

    call MPI_Finalize()
end program matrix_by_cols_p2p
