module parallel_mod
    use :: mpi_f08
    implicit none

contains

    !---------------------------------------------------------------------------
    ! For the given rank `id` from the total number of processes `n_ids`
    ! partition an array of `size` elements and find the lower and upper bounds
    ! `l` and `u`.
    subroutine partition( id, n_ids, size, l, u )
        integer, intent(in)    :: id, n_ids, size
        integer, intent(inout) :: l, u
        integer :: q, r

        r = modulo( size, n_ids ) ! remainder
        q = (size - r) / n_ids    ! quotient
        l = 1 + q * (id    ) + min( r, id     )
        u =     q * (id + 1) + min( r, id + 1 )
    end subroutine partition
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
    !---------------------------------------------------------------------------

end module parallel_mod