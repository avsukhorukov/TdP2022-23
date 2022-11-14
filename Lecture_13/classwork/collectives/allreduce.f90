! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 allreduce.f90
!
! Run with
!
!   $ mpirun -np 3 --oversubscribe ./a.out
!
! Enter the size s = 3.  Try MPI_SUM, MPI_PROD, and MPI_MAX.
program allreduce
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print
    implicit none
    integer :: my_rank, n_ranks, i, s
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    s = 0
    if (my_rank == 0) then
        print "(a)", "Enter the buffer size:"
        read *, s
    end if
    call MPI_Bcast( s, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )
    allocate(rbuf(s), source=0)

    allocate(sbuf(s))
    sbuf(:) = [ (10 * my_rank + i,  i=1, s) ]

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )

    call MPI_Allreduce( sbuf, rbuf, s, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD )

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()
end program allreduce
