! Run with -np 5 and enter s_size=10.
program scatter
    use :: mpi_f08
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
    if (my_rank == root) sbuf(:) = [ (i, i = 1, s_size) ]

    ! Broadcast the array size and create the receive buffer `sbuf` of size
    ! r_size = s / n.  Initialize it to 0 at all ranks.
    call MPI_Bcast( s_size, 1, MPI_INTEGER, root, MPI_COMM_WORLD )
    r_size = s_size / n_ranks
    allocate(rbuf(r_size), source=0)

    call print_barrier()

    call MPI_Scatter( sbuf, r_size, MPI_INTEGER, &
                      rbuf, r_size, MPI_INTEGER, root, MPI_COMM_WORLD )

    call print_barrier()

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()

contains

    subroutine print_barrier()
        integer :: rank

        if (my_rank == 0) print *
        do rank = 0, n_ranks - 1
            if (rank == my_rank) then
                print "(a, i0, a, *(i3))", "Rank ", rank, ", sbuf(:) = ", sbuf
            end if
            call MPI_Barrier( MPI_COMM_WORLD )
        end do
        do rank = 0, n_ranks - 1
            if (rank == my_rank) then
                print "(a, i0, a, *(i3))", "Rank ", rank, ", rbuf(:) = ", rbuf
            end if
            call MPI_Barrier( MPI_COMM_WORLD )
        end do
    end subroutine print_barrier

end program scatter
