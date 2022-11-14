module parallel_mod
    use :: mpi_f08
    implicit none

contains

    !---------------------------------------------------------------------------
    ! Create a 1D open topology.
    integer function get_open_rank( rnk, n_ids )
        integer, intent(in) :: rnk, n_ids

        if (0 <= rnk .and. rnk < n_ids) then
            get_open_rank = rnk
        else
            get_open_rank = MPI_PROC_NULL
        end if
    end function get_open_rank

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

end module parallel_mod