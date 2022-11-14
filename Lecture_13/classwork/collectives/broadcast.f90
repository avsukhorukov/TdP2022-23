! Rank 0 reads in the array size.  This size is sent to the other ranks.  Later
! change this to a broadcast.  All ranks allocate the same array and set its
! values to zero.  Rank 0 populates this array with some arithmetic progression.
! This array is broacast to all ranks.
!
! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace broadcast.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
program broadcast
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print
    implicit none
    integer :: my_rank, n_ranks, root, arr_size, i
    integer, allocatable :: arr(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    root = 0

    ! Size s must be zero at ranks but 0, where it is read from the terminal.
    arr_size = 0
    call barrier_print( arr_size, "arr_size", my_rank, n_ranks, MPI_COMM_WORLD )
    if (my_rank == root) print *

    if (my_rank == root) then
        print "(a)", "Enter the array size:"
        read *, arr_size
    end if
    call barrier_print( arr_size, "arr_size", my_rank, n_ranks, MPI_COMM_WORLD )
    if (my_rank == root) print *

    call MPI_Bcast( arr_size, 1, MPI_INTEGER, root, MPI_COMM_WORLD )

    call barrier_print( arr_size, "arr_size", my_rank, n_ranks, MPI_COMM_WORLD )
    if (my_rank == root) print *

    allocate(arr(arr_size), source=0)
    if (my_rank == root) then
        arr(:) = [ (i,  i=1, arr_size) ]
    end if
    call barrier_print( arr, "arr", my_rank, n_ranks, MPI_COMM_WORLD )
    if (my_rank == root) print *
    
    call MPI_Bcast( arr, 1, MPI_INTEGER, root, MPI_COMM_WORLD ) ! try a(3), count=2 and so on.

    call barrier_print( arr, "arr", my_rank, n_ranks, MPI_COMM_WORLD )
    if (my_rank == root) print *

    if (allocated(arr)) deallocate(arr)
    call MPI_Finalize()
end program broadcast
