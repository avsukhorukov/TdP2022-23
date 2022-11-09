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

end module parallel_mod