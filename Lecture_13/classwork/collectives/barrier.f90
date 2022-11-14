! Rank 0 reads in the array size.  This size is sent to the other ranks.  All
! ranks allocate the same array and set its values to zero.  Rank 0 populates
! this array with some arithmetic progression.
!
! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 barrier.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
program barrier
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print
    implicit none
    type(MPI_Status) :: status
    integer :: my_rank, n_ranks, i, arr_size
    integer, allocatable :: arr(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    arr_size = 0
    if (my_rank == 0) then
        print "(a)", "Enter the array size:"
        read *, arr_size
        do i = 1, n_ranks - 1
            call MPI_Send( arr_size, 1, MPI_INTEGER, i, 0, MPI_COMM_WORLD )
        end do
    else
        call MPI_Recv( arr_size, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status )
    end if
    allocate(arr(arr_size), source=0)

    call barrier_print( arr, "arr", my_rank, n_ranks, MPI_COMM_WORLD )

    if (my_rank == 0) then
        arr(:) = [ (i,  i=1, arr_size) ]
        print "(a)", "Press any key"
        read *
    end if
    call MPI_Barrier( MPI_COMM_WORLD )

    call barrier_print( arr, "arr", my_rank, n_ranks, MPI_COMM_WORLD )

    if (allocated(arr)) deallocate(arr)
    call MPI_Finalize()
end program barrier
