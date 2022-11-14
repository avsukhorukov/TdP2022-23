! Identity matrix distribution
!
! Write a program that distributes an identity matrix over the processes.  The
! dimension and the number of processes are given.  Distribute by rows (in
! Fortran).  The program reads the matrix size S, takes the number of processes
! N, and calculates how many rows each process must have.  Better if subarrays,
! local to each rank, use through indexing.  At the end, rank 0 collects the
! matrix rows from the other ranks and prints the entire matrix.
!
! Compile and run:
!
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!   parallel_mod.f90 matrix_by_rows_collectives.f90
! $ mpirun -np 3 --oversubscribe ./a.out
!
! Enter the size of 10.
program matrix_by_rows_collectives
    use :: mpi_f08
    use :: parallel_mod, only : partition
    implicit none
    integer              :: s       ! matrix size
    integer, allocatable :: e(:, :) ! matrix itself
    integer :: my_rank, n_ranks
    integer :: i, ib, ie
    type(MPI_Status) :: status
    type(MPI_Datatype) :: a_row

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    if (my_rank == 0) then
        print "(a)", "Enter the matrix size S:"
        read *, s
    end if
    call MPI_Bcast( s, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )

    call partition( my_rank, n_ranks, s, ib, ie )

    ! Initialize the local matrix.
    allocate(e(ib:ie, s), source=0)
    do i = ib, ie
        e(i, i) = 1
    end do

    ! Define a datatype for a full matrix row and resize it to the extent of
    ! one matrix element of type integer.
    block
        type(MPI_Datatype) :: a_tmp_row
        integer(kind=MPI_ADDRESS_KIND) :: lb, extent

        call MPI_Type_get_extent( MPI_INTEGER, lb, extent )
        call MPI_Type_vector( s, 1, ie - ib + 1, MPI_INTEGER, a_tmp_row )
        call MPI_Type_create_resized( a_tmp_row, lb, extent, a_row )
        call MPI_Type_commit( a_row )
    end block

!+  Possible MPI_Gatherv, but into a big array at rank 0.  This fast but memory
!   consuming for big inputs.
    if (my_rank == 0) then
        block
            integer :: rank
            do rank = 0, n_ranks - 1
                if (rank /= 0) &
                    call MPI_Recv( e(1, 1), ie - ib + 1, a_row, rank, 0, MPI_COMM_WORLD, status )
                print "(a, i2)", "Rank ", rank
                do i = 1, quotient + merge(1, 0, rank < remainder)
                    print "(*(i2))", e(i, :)
                end do
            end do
        end block
    else
        call MPI_Send( e(ib, 1), ie - ib + 1, a_row, 0, 0, MPI_COMM_WORLD )
    end if
!-

    call MPI_Type_free( a_row )
    deallocate(e)

    call MPI_Finalize()
end program matrix_by_rows_collectives
