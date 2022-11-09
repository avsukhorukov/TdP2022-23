module parallel_mod
    use :: mpi_f08
    implicit none

contains

    !---------------------------------------------------------------------------
    ! Print 2D array `arr` from each rank in consecutive order using the barrier
    ! collective.  Warning: serial output might not work in some infrequent
    ! cases.
    subroutine barrier_print_2d( arr, my_id, n_ids, comm )
        integer,        intent(in) :: arr(:, :), my_id, n_ids
        type(MPI_Comm), intent(in) :: comm
        integer :: rank, row

        do rank = 0, n_ids
            if (rank == my_id) then
                print '(a, i2)', "Rank ", my_id
                do row = 1, size( arr, dim=1 )
                    print '(*(i3))', arr(row, :)
                end do
            end if
            call MPI_Barrier( comm )
        end do
    end subroutine barrier_print_2d
    !---------------------------------------------------------------------------
    subroutine partition( id, n_ids, size, b, e )
        integer, intent(in)    :: id, n_ids, size
        integer, intent(inout) :: b, e
        integer :: remainder, quotient

        remainder = modulo( size, n_ids )
        quotient  = (size - remainder) / n_ids
        b = 1 + quotient * (id    ) + min( remainder, id     )
        e =     quotient * (id + 1) + min( remainder, id + 1 )
    end subroutine partition
    !---------------------------------------------------------------------------
    integer function get_rank( rnk, n_ids )
        integer, intent(in) :: rnk, n_ids
        get_rank = modulo( rnk, n_ids )
    end function get_rank
    !---------------------------------------------------------------------------

end module parallel_mod