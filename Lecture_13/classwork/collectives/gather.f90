! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 gather.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
!
! Enter a multiple of -np 5 for s_size.
program gather
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print
    implicit none
    integer :: my_rank, n_ranks, root, r_size, s_size, i
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    root = 0

    ! At the root, read the receive size `r_size` and create the receive buffer
    ! `rbuf(r_size)`.  At the other ranks, make if zero-sized.
    r_size = 0
    if (my_rank == root) then
        print "(a)", "Enter the receive buffer size (must be a multiple of -np):"
        read *, r_size
    end if
    allocate(rbuf(r_size), source=0)

    ! Broadcast `r_size` to calculate the size of the send buffer at each rank.
    ! The receive buffer size is the send buffer size times the number of
    ! processes.
    call MPI_Bcast( r_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )
    s_size = r_size / n_ranks

    allocate(sbuf(s_size))
    sbuf(:) = [ (10 * my_rank + i,  i=1, s_size) ]

    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    if (my_rank == root) print *

    call MPI_Gather( sbuf, s_size, MPI_INTEGER, &
                     rbuf, s_size, MPI_INTEGER, root, MPI_COMM_WORLD )

    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    if (my_rank == root) print *

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()
end program gather
