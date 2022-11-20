! Parallel sum of a 1D array, evenly distributed between all ranks.
!
! Compile and run:
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_sum_even.f90
!   $ mpirun -np 4 --oversubscribe ./a.out < in16.txt
!   136
!
! Note: students are confused if they are asked to send the local size of a
! subarray.  It is better to send the global size and let each process calculate
! its own size and indices.
program parallel_sum_even
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, rank
    integer :: full_size, net_sum, b, e
    integer, allocatable :: arr(:)
    type(MPI_Status) :: status

    call MPI_Init()

    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    ! Read in and broadcast the array size to other ranks.  In a more general
    ! case, when the array cannot be divided evenly between all the ranks, it
    ! is better to send the full size to them all so as each of them can
    ! compute the local size and the beginning index of the subarray.
    if (my_rank == 0) then
        read *, full_size
        do rank = 1, n_ranks - 1
            call MPI_Send( full_size, 1, MPI_INTEGER, rank, 0, MPI_COMM_WORLD )
        end do
    else
        call MPI_Recv( full_size, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status )
    end if

    ! Create local sub-arrays with through indexing.
    call partition( my_rank, n_ranks, full_size, b, e )
    allocate(arr(b:e))

    ! Read in and distribute the full array.
    if (my_rank == 0) then
        block
            integer, allocatable :: full_arr(:)
            allocate(full_arr(full_size))
            read *, full_arr(:)
            ! Send all chunks but first to the other ranks.
            do rank = 1, n_ranks - 1
                call partition( rank, n_ranks, full_size, b, e )
                call MPI_Send( full_arr(b), e - b + 1, MPI_INTEGER, rank, 0, MPI_COMM_WORLD )
            end do
            ! Copy the first chunk directly.
            call partition( 0, n_ranks, full_size, b, e )
            arr(b:e) = full_arr(b:e)
            deallocate(full_arr)
        end block
    else
        call MPI_Recv( arr(b), e - b + 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status )
    end if

    ! Calculate local sums.
    net_sum = sum(arr(:))
    deallocate(arr)

    ! Add up sums at rank 0 and print the result.
    if (my_rank == 0) then
        block
            integer :: full_sum
            full_sum = net_sum ! copy from rank 0
            do rank = 1, n_ranks - 1
                call MPI_Recv( net_sum, 1, MPI_INTEGER, rank, 0, MPI_COMM_WORLD, status )
                full_sum = full_sum + net_sum
            end do
            print "(i0)", full_sum
        end block
    else
        call MPI_Send( net_sum, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD )
    end if

    call MPI_Finalize()

contains

    ! For the given rank `id` from the total number of processes `n_ids`
    ! partition an array of `size` elements and find the lower and upper bounds
    ! `l` and `u`.
    subroutine partition( id, n_ids, size, l, u )
        integer, intent(in)    :: id, n_ids, size
        integer, intent(inout) :: l, u
        integer :: q

        q = size / n_ids ! quotient
        l = 1 + q * (id    )
        u =     q * (id + 1)
    end subroutine partition

end program parallel_sum_even