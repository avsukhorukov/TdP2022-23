! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 alltoall.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
!
! 1) Enter 5 (it is easy to demonstrate) for the buffer size;
! 2) Enter 10.
program alltoall
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print
    implicit none
    integer :: my_rank, n_ranks, s, i, count
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    ! s is the size of both the receive buffer and the send buffer.
    if (my_rank == 0) then
        print "(a)", "Enter the buffer size (both send and receive, multiple of -np):"
        read *, s
    end if
    call MPI_Bcast( s, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )
    allocate(sbuf(s))
    sbuf(:) = [ (10 * my_rank + i,  i=1, s) ]
    allocate(rbuf(s), source=0)

    count = s / n_ranks

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )

    call MPI_Alltoall( sbuf, count, MPI_INTEGER, &
                       rbuf, count, MPI_INTEGER, MPI_COMM_WORLD )

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()
end program alltoall
