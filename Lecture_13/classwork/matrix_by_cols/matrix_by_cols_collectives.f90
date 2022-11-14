! Matrix distribution
!
! Distribute a global N x N matrix over P processes as evenly as possible so
! that each process has a local portion of this matrix.  Initialize such a
! portion with the process rank.  N is not necessarily divisible by P.  Each
! process sends its first and last columns (in Fortran) to neighbors: first to
! the left rank, last to the right rank.  Each process must allocate two extra
! colums with ghost cells.
!
! Compile and run:
!
!     $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!       matrix_by_cols_collective.f90
!     $ mpirun -np 3 --oversubscribe ./a.out
!
! Enter size 10.
program matrix_by_cols_collectives
    use :: mpi_f08
    use :: parallel_mod, only : get_rank, partition, barrier_print_2d !, print_gatherv
    implicit none
    integer              :: s            ! matrix size
    integer, allocatable :: matrix(:, :) ! matrix itself
    integer :: my_rank, n_ranks, left_rank, right_rank, jb, je
    
    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    left_rank  = get_rank( my_rank - 1, n_ranks )
    right_rank = get_rank( my_rank + 1, n_ranks )

    if (my_rank == 0) then
        print "(a)", "Enter the matrix size:"
        read *, s
    end if
    call MPI_Bcast( s, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )

    call partition( my_rank, n_ranks, s, jb, je )

    allocate(matrix(s, jb - 1:je + 1), source=my_rank)

    call barrier_print_2d( matrix, my_rank, n_ranks, MPI_COMM_WORLD )
    ! call print_gatherv( matrix, my_rank, n_ranks, MPI_COMM_WORLD )

    if (my_rank == 0) read *
    call MPI_Barrier( MPI_COMM_WORLD )

    ghost_exchange: block
        type(MPI_Status) :: status
        type(MPI_Datatype) :: a_col

        call MPI_Type_contiguous( s, MPI_INTEGER, a_col )
        call MPI_Type_commit( a_col )

        call MPI_Sendrecv( matrix(1, je    ), 1, a_col, right_rank, 0, &
                           matrix(1, jb - 1), 1, a_col, left_rank,  0, MPI_COMM_WORLD, status )
        call MPI_Sendrecv( matrix(1, jb    ), 1, a_col, left_rank,  1, &
                           matrix(1, je + 1), 1, a_col, right_rank, 1, MPI_COMM_WORLD, status )

        call MPI_Type_free( a_col )
    end block ghost_exchange

    call barrier_print_2d( matrix, my_rank, n_ranks, MPI_COMM_WORLD )
    ! call print_gatherv( matrix, my_rank, n_ranks, MPI_COMM_WORLD )

    deallocate(matrix)
    call MPI_Finalize()
end program matrix_by_cols_collectives
