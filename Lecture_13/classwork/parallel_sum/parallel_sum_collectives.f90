! Parallel sum of a 1D array, either evenly or unevenly distributed between all
! ranks.
!
! Compile and run:
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 parallel_sum_collectives.f90
!
!   $ mpirun -np 4 --oversubscribe ./a.out < in16.txt
!   136
!
!   $ mpirun -np 4 --oversubscribe ./a.out < in17.txt
!   153
!
program parallel_sum_collectives
    use :: mpi_f08
    use :: parallel_mod, only : barrier_print_1d
    implicit none
    integer :: my_rank, n_ranks
    integer :: full_size, net_sum, b, e
    integer, allocatable :: arr(:)

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    ! Read in and broadcast the array size to other ranks.  In a more general
    ! case, when the array cannot be divided evenly between all the ranks, it
    ! is better to send the full size to them all so as each of them can
    ! compute the local size and the beginning index of the subarray.
    if (my_rank == 0) then
        read *, full_size
    end if
    call MPI_Bcast( full_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )

    !---------------------------------------------------------------------------
    ! The array is evenly divisible by the number of processes.  Scatter it to
    ! other processes.
    even_scatter: block
        integer :: size
        integer, allocatable :: full_arr(:)

        size = full_size / n_ranks
        b = 1 + size * (my_rank    )
        e =     size * (my_rank + 1)
        allocate(arr(b:e))

        if (my_rank == 0) then
            allocate(full_arr(full_size))
            read *, full_arr(:)
        else
            allocate(full_arr(0))
        end if

        call MPI_Scatter( full_arr, size, MPI_INTEGER, &
                          arr,      size, MPI_INTEGER, 0, MPI_COMM_WORLD )

        if (allocated(full_arr)) deallocate(full_arr)
    end block even_scatter
    !---------------------------------------------------------------------------
    ! The array is not evenly divisible by the number of processes.  Calculate
    ! sizes and displacements at each rank and vector-scatter the array to other
    ! processes.
    uneven_scatterv: block
        integer, allocatable :: full_arr(:), sizes(:), disps(:)
        integer :: remainder, quotient, disp, size, rank

        remainder = modulo( full_size, n_ranks )
        quotient  = (full_size - remainder) / n_ranks

        allocate(sizes(0:n_ranks - 1))
        allocate(disps(0:n_ranks - 1))
        disp = 0
        do rank = 0, n_ranks - 1
            sizes(rank) = quotient + merge( 1, 0, rank < remainder )
            disps(rank) = disp
            disp = disp + sizes(rank)
            if (rank == my_rank) then
                size = sizes(rank)
                b    = disps(rank) + 1
                e    = disps(rank) + size
            end if
        end do
        allocate(arr(b:e))

        if (my_rank == 0) then
            allocate(full_arr(full_size))
            read *, full_arr(:)
        else
            allocate(full_arr(0))
        end if

        call MPI_Scatterv( full_arr, sizes, disps, MPI_INTEGER, &
                           arr,      size,         MPI_INTEGER, 0, MPI_COMM_WORLD )

        if (allocated(full_arr)) deallocate(full_arr)
        if (allocated(sizes)) deallocate(sizes)
        if (allocated(disps)) deallocate(disps)
    end block uneven_scatterv
    !---------------------------------------------------------------------------
    ! The arrray is not evenly divisible.  Each process calculates the local
    ! size, then processes gather to all sizes.
    allgather: block
        integer, allocatable :: full_arr(:), sizes(:), disps(:)
        integer :: remainder, quotient, size, disp, rank

        remainder = modulo( full_size, n_ranks )
        quotient  = (full_size - remainder) / n_ranks

        size = quotient + merge( 1, 0, my_rank < remainder )

        allocate(sizes(0:n_ranks - 1))
        call MPI_Allgather( size,  1, MPI_INTEGER, &
                            sizes, 1, MPI_INTEGER, MPI_COMM_WORLD )

        allocate(disps(0:n_ranks - 1))
        disp = 0
        do rank = 0, n_ranks - 1
            disps(rank) = disp
            disp = disp + sizes(rank)
        end do
        b = disps(my_rank) + 1
        e = disps(my_rank) + size
        allocate(arr(b:e))

        if (my_rank == 0) then
            allocate(full_arr(full_size))
            read *, full_arr(:)
        else
            allocate(full_arr(0))
        end if

        call MPI_Scatterv( full_arr, sizes, disps, MPI_INTEGER, &
                           arr,      size,         MPI_INTEGER, 0, MPI_COMM_WORLD )

        if (allocated(sizes)) deallocate(sizes)
        if (allocated(disps)) deallocate(disps)
        if (allocated(full_arr)) deallocate(full_arr)
    end block allgather
    !---------------------------------------------------------------------------
    ! The array is not evenly divisible.  Each process calculates the local
    ! size, then all perform exclusive scan to find the local displacement.
    ! Sizes and displacements are gathered at root.  The full array is vector-
    ! scattered.
    exscan: block
        integer, allocatable :: full_arr(:), sizes(:), disps(:)
        integer :: remainder, quotient, size, disp

        if (my_rank == 0) then
            allocate(full_arr(full_size))
            read *, full_arr(:)
            allocate(sizes(0:n_ranks - 1))
            allocate(disps(0:n_ranks - 1))
        else
            allocate(full_arr(0))
            allocate(sizes(0))
            allocate(disps(0))
        end if

        remainder = modulo( full_size, n_ranks )
        quotient  = (full_size - remainder) / n_ranks
        size = quotient + merge( 1, 0, my_rank < remainder )

        disp = 0
        call MPI_Exscan( size, disp, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD )
        b    = disp + 1
        e    = disp + size
        allocate(arr(b:e))

        call MPI_Gather( size,  1, MPI_INTEGER, &
                         sizes, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )
        call MPI_Gather( disp,  1, MPI_INTEGER, &
                         disps, 1, MPI_INTEGER, 0, MPI_COMM_WORLD )
        call MPI_Scatterv( full_arr, sizes, disps, MPI_INTEGER, &
                           arr,      size,         MPI_INTEGER, 0, MPI_COMM_WORLD )

        if (allocated(sizes)) deallocate(sizes)
        if (allocated(disps)) deallocate(disps)
        if (allocated(full_arr)) deallocate(full_arr)
    end block exscan
    !---------------------------------------------------------------------------

    call barrier_print_1d( arr, my_rank, n_ranks, MPI_COMM_WORLD )

    ! Calculate local sums.
    net_sum = sum( arr(:) )
    deallocate(arr)

    ! Add up sums at rank 0 and print the result.
    block
        integer :: full_sum

        call MPI_Reduce( net_sum, full_sum, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD )
        if (my_rank == 0) print "(i0)", full_sum
    end block

    call MPI_Finalize()
end program parallel_sum_collectives