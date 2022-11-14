module parallel_mod
    use :: mpi_f08
    implicit none

contains

    !---------------------------------------------------------------------------
    ! Note: this slave-to-master I/O of a scalar, 1D, or 2D array is
    ! memory-consuming.  The allocatable `x_out` has the size of one `x`, which
    ! is critical if `x` is big.  The right thing to do is to send `x` by rows
    ! to rank 0, which has to print every array row-by-row.
    subroutine serial_print( x, x_name, my_id, n_ids, comm )
        integer, dimension(..), intent(in) :: x
        character(len=*),       intent(in) :: x_name
        integer,                intent(in) :: my_id, n_ids
        type(MPI_Comm),         intent(in) :: comm

        if (my_id == 0) then
            block
                integer              :: id, i
                type(MPI_Status)     :: status
                integer, allocatable :: x_out(:, :)
                !
                select rank (x)
                rank (0)
                    allocate(x_out(1, 1), source=x)
                rank (1)
                    allocate(x_out(size( x, dim=1 ), 1))
                    x_out(:, 1) = x(:)
                rank (2)
                    allocate(x_out(size( x, dim=1 ), size( x, dim=2 )))
                    x_out(:, :) = x(:, :)
                rank default
                    stop "serial_print: rank higher than 2 for the 1st argument is not implemented."
                end select

                do id = 0, n_ids - 1
                    if (id /= 0) then
                        call MPI_Recv( x_out, size( x_out ), MPI_INTEGER, id, 0, comm, status )
                        call MPI_Get_count( status, MPI_INTEGER, i )
                        ! If nothing has been sent (the sendbuf has size 0),
                        ! then go to the next rank.
                        if (i == 0) cycle
                    end if
                    write(*, "(a, i3, 3a)", advance="no") "Rank ", id, ", ", x_name, " = "
                    select rank (x)
                    rank (0)
                        print "(i3)", x_out
                    rank (1)
                        print "(*(i3))", x_out(:, 1)
                    rank (2)
                        print *
                        do i = 1, size( x_out, dim=1 )
                            print "(*(i3))", x_out(i, :)
                        end do
                    rank default
                        stop "serial_print: rank higher than 2 for the 1st argument is not implemented."
                    end select
                end do
                deallocate(x_out)
            end block
        else
            call MPI_Send( x, size( x ), MPI_INTEGER, 0, 0, comm )
        end if
    end subroutine serial_print
    !---------------------------------------------------------------------------
    ! Print a scalar, 1D or 2D array `x` from each rank in consecutive order
    ! using the barrier collective.  Warning: serial output might not work in
    ! some infrequent cases.
    subroutine barrier_print( x, x_name, my_id, n_ids, comm )
        integer, dimension(..), intent(in) :: x
        character(len=*),       intent(in) :: x_name
        integer,                intent(in) :: my_id, n_ids
        type(MPI_Comm),         intent(in) :: comm

        integer :: id

        do id = 0, n_ids
            if (id == my_id) then
                select rank (x)
                rank (0)
                    print "(a, i3, 3a, i3)", "Rank ", id, ", ", x_name, " = ", x
                rank (1)
                    print "(a, i3, 3a, *(i3))", "Rank ", id, ", ", x_name, "(:) = ", x
                rank (2)
                    print "(a, i3, 3a)", "Rank ", id, ", ", x_name, "(:, :) = "
                    block
                        integer :: row
                        do row = 1, size( x, dim=1 )
                            print "(*(i3))", x(row, :)
                        end do
                    end block
                rank default
                    stop "barrier_print: ranks higher than 2 are not supported for the first argument."
                end select
            end if
            call MPI_Barrier( comm )
        end do
    end subroutine barrier_print
    !---------------------------------------------------------------------------
    subroutine print_gatherv( mtx, id, n_ids, comm )
        integer,        intent(in) :: mtx(:, :), id, n_ids
        type(MPI_Comm), intent(in) :: comm
        character(len=:), allocatable :: line, lines
        character(len=12) :: i_fmt ! integer format string
        integer :: line_len, lines_len, i, n_rows, n_cols, n_proc, disp
        integer, parameter :: i_width = 1 ! "(i2)" descriptor
        integer, allocatable :: sizes(:), disps(:)

        write(unit=i_fmt, fmt="(a, i0, a)") "(*(i", i_width,", 1x))"

        n_rows = size( mtx, dim=1 )
        n_cols = size( mtx, dim=2 )
        line_len = (i_width + 1) * (1 + n_cols)
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
    integer function get_ring_rank( rnk, n_ids )
        integer, intent(in) :: rnk, n_ids

        get_ring_rank = modulo( rnk, n_ids )
    end function get_ring_rank
    !---------------------------------------------------------------------------
    integer function get_open_rank( rnk, n_ids )
        integer, intent(in) :: rnk, n_ids

        if (0 <= rnk .and. rnk < n_ids) then
            get_open_rank = rnk
        else
            get_open_rank = MPI_PROC_NULL
        end if
    end function get_open_rank
    !---------------------------------------------------------------------------

end module parallel_mod