! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 reduce.f90
!
! Run with
!
!   $ mpirun -np 3 --oversubscribe ./a.out
!
! Enter the size s = 2--3.  Try MPI_SUM, MPI_PROD, and MPI_MAX.
program reduce
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print
    implicit none
    integer :: my_rank, n_ranks, root
    integer, allocatable :: rbuf(:), sbuf(:)
    integer :: i, s

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    root = 0

    s = 0
    if (my_rank == root) then
        print "(a)", "Enter the buffer size:"
        read *, s
    end if
    allocate(rbuf(s), source=0)

    call MPI_Bcast( s, 1, MPI_INTEGER, root, MPI_COMM_WORLD )

    allocate(sbuf(s))
    sbuf(:) = [ (10 * my_rank + i,  i=1, s) ]

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )

    call MPI_Reduce( sbuf, rbuf, s, MPI_INTEGER, MPI_SUM, root, MPI_COMM_WORLD )
    ! call MPI_Reduce( sbuf, rbuf, s, MPI_INTEGER, MPI_PROD, root, MPI_COMM_WORLD )
    ! call MPI_Reduce( sbuf, rbuf, s, MPI_INTEGER, MPI_MAX, root, MPI_COMM_WORLD )

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()
end program reduce
