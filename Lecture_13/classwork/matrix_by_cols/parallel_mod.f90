module parallel_mod
    use :: mpi_f08
    implicit none

contains

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
    subroutine print_gatherv( mtx, id, n_ids, comm )
        integer,        intent(in) :: mtx(:, :), id, n_ids
        type(MPI_Comm), intent(in) :: comm
        character(len=:), allocatable :: line, lines
        character(len=12) :: i_fmt ! integer format string
        integer :: line_len, lines_len, i, n_rows, n_cols, n_proc, disp
        integer, parameter :: i_width = 3 ! "(i3)" descriptor
        integer, allocatable :: sizes(:), disps(:)

        write(unit=i_fmt, fmt="(a, i0, a)") "(*(i", i_width,"))"

        n_rows = size( mtx, dim=1 )
        n_cols = size( mtx, dim=2 )
        line_len = i_width * (1 + n_cols)
        lines_len = 0
        call MPI_Reduce( line_len, lines_len, 1, MPI_INTEGER, MPI_SUM, 0, comm )
        allocate(character(len=line_len)  :: line)
        allocate(character(len=lines_len) :: lines)

        disp = 0
        call MPI_Exscan( line_len, disp, 1, MPI_INTEGER, MPI_SUM, comm )

        n_proc = merge( n_ids, 0, id == 0 )
        allocate(sizes(n_proc))
        allocate(disps(n_proc))
        call MPI_Gather( line_len, 1, MPI_INTEGER, sizes, 1, MPI_INTEGER, 0, comm )
        call MPI_Gather( disp,     1, MPI_INTEGER, disps, 1, MPI_INTEGER, 0, comm )

        do i = 0, n_rows
            if (i == 0) then
                write(line, "(a, i0)") "Rank ", id
            else
                write(line, i_fmt) mtx(i, :)
            end if
            call MPI_Gatherv( line,  line_len,     MPI_CHARACTER, &
                              lines, sizes, disps, MPI_CHARACTER, 0, comm )
            if (id == 0) print "(a)", lines
        end do

        if (allocated(line)) deallocate(line)
        if (allocated(lines)) deallocate(lines)
        if (allocated(sizes)) deallocate(sizes)
        if (allocated(disps)) deallocate(disps)
    end subroutine print_gatherv
    !---------------------------------------------------------------------------

end module parallel_mod