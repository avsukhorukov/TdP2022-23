module parallel_mod
    use :: mpi_f08
    implicit none

contains

    ! Note: this slave-to-master I/O of a 2D array is memory-consuming.  The
    ! array `arr_out` has the size of one array `arr`, which is critical if
    ! `arr` is big.  The right thing to do is to send `arr` by rows to rank 0,
    ! which has to print every array row-by-row.
    subroutine serial_print( arr, my_id, n_ids, comm )
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
    end subroutine serial_print

end module parallel_mod