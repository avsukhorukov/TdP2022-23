module parallel_mod
    use :: mpi_f08
    implicit none

contains

    ! For the given rank `id` from the total number of processes `n_ids`
    ! partition an array of `size` elements and find the lower and upper bounds
    ! `l` and `u`.
    subroutine partition( id, n_ids, size, l, u )
        integer, intent(in)    :: id, n_ids, size
        integer, intent(inout) :: l, u
        integer :: q
#if defined (EVEN)
        q = size / n_ids ! quotient
        l = 1 + q * (id    )
        u =     q * (id + 1)
#elif defined (UNEVEN)
        ! TODO
#endif
    end subroutine partition

    !---------------------------------------------------------------------------
    ! Note: this slave-to-master I/O of a 2D array is memory-consuming.  The
    ! array `arr_out` has the size of one array `arr`, which is critical if
    ! `arr` is big.  The right thing to do is to send `arr` by rows to rank 0,
    ! which has to print every array row-by-row.
    subroutine serial_print_2d( arr, my_id, n_ids, comm )
        integer,        intent(in) :: arr(:, :), my_id, n_ids
        type(MPI_Comm), intent(in) :: comm

        if (my_id == 0) then
            block
                integer              :: i, rank
                type(MPI_Status)     :: status
                integer, allocatable :: arr_out(:, :)
                !
                allocate(arr_out, mold=arr)
                do rank = 0, n_ids - 1
                    if (rank == 0) then
                        arr_out(:, :) = arr(:, :)
                    else
                        call MPI_Recv( arr_out, size( arr_out ), MPI_INTEGER, rank, 0, comm, status )
                    end if
                    print "(a, i0)", "Rank ", rank
                    do i = 1, size( arr_out, dim=1 )
                        print "(*(i2, :, 1x))", arr_out(i, :)
                    end do
                end do
                deallocate(arr_out)
            end block
        else
            call MPI_Send( arr, size( arr ), MPI_INTEGER, 0, 0, comm )
        end if
    end subroutine serial_print_2d

    !---------------------------------------------------------------------------
    ! Print 1D array `arr` from each rank in consecutive order using the barrier
    ! collective.  Warning: serial output might not work in some infrequent
    ! cases.
    subroutine barrier_print_1d( arr, my_id, n_ids, comm )
        integer,        intent(in) :: arr(:), my_id, n_ids
        type(MPI_Comm), intent(in) :: comm
        integer :: rank

        do rank = 0, n_ids
            if (rank == my_id) then
                print "(a, i2, a, *(i3, :, 1x))", "Rank ", rank, ":", arr
            end if
            call MPI_Barrier( comm )
        end do
    end subroutine barrier_print_1d

end module parallel_mod