! Compile with
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 scatterv.f90
!
! Run with
!
!   $ mpirun -np 5 --oversubscribe ./a.out
!
! Enter 18 for s_size.
program scatterv
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print
    implicit none
    integer :: my_rank, n_ranks, root
    integer :: s, i, remainder, quotient, displacement, count, rank, b, e
    integer, allocatable :: rbuf(:), sbuf(:)
    integer, allocatable :: counts(:), displacements(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    root = 0

    ! The root reads the size `s` and creates the send buffer `sbuf(s)` in full
    ! size.  Other ranks create it in zero size.
    s = 0
    if (my_rank == root) then
        print "(a)", "Enter the send buffer size (can be any):"
        read *, s
    end if
    allocate(sbuf(s))
    if (my_rank == root) sbuf(:) = [ (i,  i=1, s) ]

    call MPI_Bcast( s, 1, MPI_INTEGER, root, MPI_COMM_WORLD )

    remainder = modulo(s, n_ranks)
    quotient  = (s - remainder) / n_ranks

    allocate(counts(0:n_ranks - 1))         ! be careful with array bounds
    allocate(displacements(0:n_ranks - 1))
    displacement = 0
    do rank = 0, n_ranks - 1
        counts(rank)        = quotient + merge( 1, 0, rank < remainder )
        displacements(rank) = displacement
        displacement        = displacement + counts(rank)
        if (rank == my_rank) then
            count = counts(rank)
            b = displacements(rank) + 1
            e = displacements(rank) + count
        end if
    end do

    allocate(rbuf(b:e), source=0)

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )

    call MPI_Scatterv( sbuf, counts, displacements, MPI_INTEGER, &
                       rbuf, count,                 MPI_INTEGER, root, MPI_COMM_WORLD )

    if (my_rank == 0) print *
    call barrier_print( sbuf, "sbuf", my_rank, n_ranks, MPI_COMM_WORLD )
    call barrier_print( rbuf, "rbuf", my_rank, n_ranks, MPI_COMM_WORLD )
                   
    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)
    if (allocated(counts)) deallocate(counts)
    if (allocated(displacements)) deallocate(displacements)

    call MPI_Finalize()
end program scatterv
