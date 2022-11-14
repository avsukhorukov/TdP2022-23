! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 allgather.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
!
! Enter 5 or 10 for s_size.
program allgather
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print
    implicit none
    integer :: my_rank, n_ranks, s, i, count
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    ! The size of the receive buffer is `s`.
    if (my_rank == 0) then
        print "(a)", "Enter the receive buffer size (multiple of -np):"
        read *, s
    end if
    call MPI_Bcast( s, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )
    allocate(rbuf(s), source=0)

    ! The size of the send buffer is `s / n_ranks`.
    count = s / n_ranks
    allocate(sbuf(count))
    sbuf(:) = [ (10 * my_rank + i,  i=1, count) ]

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )

    call MPI_Allgather( sbuf, count, MPI_INTEGER, &
                        rbuf, count, MPI_INTEGER, MPI_COMM_WORLD )

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()
end program allgather
