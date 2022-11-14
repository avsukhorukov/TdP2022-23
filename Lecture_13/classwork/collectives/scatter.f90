! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 scatter.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
!
! Enter a multiple of 5 for s_size.
program scatter
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print
    implicit none
    integer :: my_rank, n_ranks, root, s_size, r_size, i
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    root = 0

    ! Allocate the send buffer `sbuf` of size `s_size` only at the root to
    ! scatter it to the other ranks.
    s_size = 0
    if (my_rank == root) then
        print "(a)", "Enter the send buffer size (must be a multiple of -np):"
        read *, s_size
    end if

    allocate(sbuf(s_size))
    if (my_rank == root) sbuf(:) = [ (i,  i=1, s_size) ]

    ! Broadcast the array size and create the receive buffer `rbuf(r_size)`.
    ! Initialize it to 0 for all ranks.
    call MPI_Bcast( s_size, 1, MPI_INTEGER, root, MPI_COMM_WORLD )
    r_size = s_size / n_ranks
    allocate(rbuf(r_size), source=0)

    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    if (my_rank == root) print *

    ! Sendcount and recvcount are the same sizes.
    call MPI_Scatter( sbuf, r_size, MPI_INTEGER, &
                      rbuf, r_size, MPI_INTEGER, root, MPI_COMM_WORLD )

    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    if (my_rank == root) print *

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()
end program scatter
