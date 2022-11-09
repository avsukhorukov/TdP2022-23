! Rank 0 reads in the array size.  This size is sent to the other ranks.  All
! ranks allocate the same array and set its values to zero.  Rank 0 populates
! this array with some arithmetic progression.
!
! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace barrier.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
program barrier
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print_1d
    implicit none
    type(MPI_Status) :: status
    integer :: my_rank, n_ranks, i, a_size
    integer, allocatable :: a(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    a_size = 0
    if (my_rank == 0) then
        print "(a)", "Enter the array size:"
        read *, a_size
        do i = 1, n_ranks - 1
            call MPI_Send( a_size, 1, MPI_INTEGER, i, 0, MPI_COMM_WORLD )
        end do
    else
        call MPI_Recv( a_size, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status )
    end if
    allocate(a(a_size), source=0)

    call barrier_print_1d( a, my_rank, n_ranks, MPI_COMM_WORLD )

    if (my_rank == 0) then
        a(:) = [ (i,  i=1, a_size) ]
        print "(a)", "Press any key"
        read *
    end if
    call MPI_Barrier( MPI_COMM_WORLD )

    call barrier_print_1d( a, my_rank, n_ranks, MPI_COMM_WORLD )

    if (allocated(a)) deallocate(a)
    call MPI_Finalize()
end program barrier
