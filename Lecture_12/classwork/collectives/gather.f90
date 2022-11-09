! Run with -np 5 or 10
program gather
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, root, r_size, s_size, i
    integer, allocatable :: rbuf(:), sbuf(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    root = 0

    ! At the root, read in the receive size `r_size` and create the receive
    ! buffef `rbuf(r_size)`.  At the oter ranks, make if zero-sized.
    r_size = 0
    if (my_rank == root) then
        print "(a)", "Enter the receive buffer size (must be a multiple of -np):"
        read *, r_size
    end if
    allocate(rbuf(r_size), source=0)

    ! Broadcast `r_size` to calculate the send size at each rank.
    call MPI_Bcast( r_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )

    s_size = r_size / n_ranks
    allocate(sbuf(s_size))
    sbuf(:) = [ (10 * my_rank + i,  i=1, s_size) ]

    call print_barrier()

    call MPI_Gather( sbuf, s_size, MPI_INTEGER, &
                     rbuf, s_size, MPI_INTEGER, root, MPI_COMM_WORLD )

    call print_barrier()

    if (allocated(sbuf)) deallocate(sbuf)
    if (allocated(rbuf)) deallocate(rbuf)

    call MPI_Finalize()

contains

    subroutine print_barrier()
        implicit none
        integer :: rank

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
        if (my_rank == 0) print *
    end subroutine print_barrier

end program gather
