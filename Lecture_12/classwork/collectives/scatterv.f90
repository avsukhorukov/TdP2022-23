! Run with -np 5 and s = 18.  Sometimes MPI_Barrier doesn't help if the output
! buffer is not flushed on time.
program scatterv
    use :: mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root
    integer :: s, i, remainder, quotient, displacement, count, rank, b, e
    integer, allocatable :: rbuf(:), sbuf(:)
    integer, allocatable :: counts(:), displacements(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    root = 0

    ! Read-in `s` on rank 0, allocate sbuf(s) only on rank 0 and later vector-
    ! scatter it to other ranks.
    s = 0
    if (my_rank == root) then
        print "(a)", "Enter the send buffer size (can be any):"
        read *, s
    end if
    allocate(sbuf(s))
    if (my_rank == root) sbuf(:) = [ (i, i = 1, s) ]

    call MPI_Bcast( s, 1, MPI_INTEGER, root, MPI_COMM_WORLD )

    remainder = modulo(s, n_ranks)
    quotient  = (s - remainder) / n_ranks

    allocate(counts(0:n_ranks - 1))
    allocate(displacements(0:n_ranks - 1))
    displacement = 0
    do rank = 0, n_ranks - 1
        if (rank < remainder) then
            counts(rank) = quotient + 1
        else
            counts(rank) = quotient
        end if
        displacements(rank) = displacement
        displacement        = displacement + counts(rank)
        if (rank == my_rank) then
            count = counts(rank)
            b = displacements(rank) + 1
            e = displacements(rank) + count
        end if
    end do

    allocate(rbuf(b:e), source=0)

    call print_vals()
    call MPI_Scatterv( sbuf, counts, displacements, MPI_INTEGER, &
                       rbuf, count,                 MPI_INTEGER, root, MPI_COMM_WORLD )
    call print_vals()

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)
    if (allocated(counts)) deallocate(counts)
    if (allocated(displacements)) deallocate(displacements)
    call MPI_Finalize()

contains

    subroutine print_vals()
        integer :: rnk

        do rnk = 0, n_ranks - 1
            if (rnk == my_rank) then
                print "(a, i0, a, *(i3))", "Rank ", rnk, ", sbuf(:) = ", sbuf
            end if
            call MPI_Barrier( MPI_COMM_WORLD )
        end do
        do rnk = 0, n_ranks - 1
            if (rnk == my_rank) then
                print "(a, i0, a, *(i3))", "Rank ", rnk, ", rbuf(:) = ", rbuf
            end if
            call MPI_Barrier( MPI_COMM_WORLD )
        end do
        if (my_rank == 0) print *
    end subroutine print_vals

end program scatterv
