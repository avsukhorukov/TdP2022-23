! Heat transport in 2D
!
! Parallel version of the serial version from heat_serial.f90.  Uses a 2D
! Cartesian topology with a 2D array distributed as evenly as possible between
! different processes.
!
! $ gfortran -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace
!   heat_parallel.f90
!
! $ ./a.out < in10.txt
program heat_parallel
    use :: mpi_f08
    implicit none
    real, dimension(:, :), pointer :: prev_t, next_t, temp_t
    integer :: s, time, ib, ie, jb, je
    real :: diff_t, global_diff_t
    integer :: n_ranks, my_rank, root, north_rank, south_rank, east_rank, west_rank
    integer :: n_rows, n_cols, row, col
    type(MPI_Datatype) :: a_row, a_col
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    root = 0

    ! Create a 2D Cartesian topology.  For simplicity, let's restrict it so far
    ! to 4 processes distributed in 2x2 blocks.
    n_rows = 2
    n_cols = 2
    if (n_rows * n_cols /= n_ranks) then
        if (my_rank == root) print "(a)", "Run with `-np 4` only"
        call MPI_Abort( MPI_COMM_WORLD, MPI_ERR_TOPOLOGY )
    end if

    ! Get the row and column indices of the current block in the 2D topology.
    call get_coords( my_rank, n_rows, n_cols, row, col )

    ! Define neighbor ranks with non-periodic boundaries.
    north_rank = get_rank( row - 1, col,     n_rows, n_cols )
    south_rank = get_rank( row + 1, col,     n_rows, n_cols )
    west_rank  = get_rank( row,     col - 1, n_rows, n_cols )
    east_rank  = get_rank( row,     col + 1, n_rows, n_cols )

    ! Read in the array side and broadcast it to all.
    if (my_rank == root) read *, s
    call MPI_Bcast( s, 1, MPI_INTEGER, root, MPI_COMM_WORLD )

    call partition( row, n_rows, s, ib, ie )
    call partition( col, n_cols, s, jb, je )

    allocate(prev_t(ib - 1:ie + 1, jb - 1:je + 1))
    allocate(next_t(ib - 1:ie + 1, jb - 1:je + 1))

    ! One column covers the internal range with no ghost cells, [ib:ie, :].
    ! For synronizing ghost columns.
    call MPI_Type_contiguous( ie - ib + 1, MPI_REAL, a_col )
    call MPI_Type_commit( a_col )

    ! One row covers the internal range and two ghost cells, [:, jb-1:je+1].
    ! Must be resized to one real element.
    block
        type(MPI_Datatype) :: a_tmp_row
        integer(kind=MPI_ADDRESS_KIND) :: lb, real_extent

        call MPI_Type_vector( je - jb + 3, 1, ie - ib + 3, MPI_REAL, a_tmp_row )
        call MPI_Type_get_extent( MPI_REAL, lb, real_extent )
        call MPI_Type_create_resized( a_tmp_row, lb, real_extent, a_row )
        call MPI_Type_commit( a_row )
    end block

    call read_temperature( prev_t, s )

    ! After reading from the input file, the ghost rows are not updated except
    ! for the first and last of the global array.  Synchronize all rows to
    ! update the missing values.
    ! call MPI_Sendrecv( prev_t(ib,     jb - 1), 1, a_row, north_rank, 3, &
    !                    prev_t(ie + 1, jb - 1), 1, a_row, south_rank, 3, MPI_COMM_WORLD, status )
    ! call MPI_Sendrecv( prev_t(ie,     jb - 1), 1, a_row, south_rank, 4, &
    !                    prev_t(ib - 1, jb - 1), 1, a_row, north_rank, 4, MPI_COMM_WORLD, status )
    call synchronize_ghost( prev_t )

    ! Copy border values from prev_t to next_t (including all ghost values).
    call update_borders( prev_t, next_t )

    ! call print_blocks( next_t )

    time = 0
    do
        call next_iteration( prev_t, next_t )
        call synchronize_ghost( next_t )

        diff_t = maxval( abs( next_t(ib:ie, jb:je) - prev_t(ib:ie, jb:je) ) )
        call MPI_Allreduce( diff_t, global_diff_t, 1, MPI_REAL, MPI_MAX, MPI_COMM_WORLD )

        if (modulo( time, 5000 ) == 0) then
            if (my_rank == root) then
                print "(a, i0, a, es8.2)", "time = ", time, ", diff_t = ", global_diff_t
            end if
            call print_blocks( prev_t )
            ! call print_temperature( prev_t, s )
            ! call wait_cls( 100 )
        end if
        if (global_diff_t < epsilon( 0.0 )) exit
        temp_t => prev_t;  prev_t => next_t;  next_t => temp_t
        time = time + 1
    end do
    if (my_rank == root) print "(a, i0)", "Final time is ", time
    deallocate(prev_t)
    deallocate(next_t)

    call MPI_Type_free( a_row )
    call MPI_Type_free( a_col )

    call MPI_Finalize()
contains
    !---------------------------------------------------------------------------
    ! Read in the global array and distribute to other processes row-by-row.
    subroutine read_temperature( mtx, side )
        real, dimension(:, :), pointer, intent(in out) :: mtx
        integer,                        intent(in)     :: side
        ! Row and column indices not to change ib, ie, jb, je.
        integer :: i, rb, re, cb, ce

        if (my_rank == root) then
            block
                real, dimension(:), allocatable :: t_row
                integer :: dst
                allocate(t_row(0:side + 1))
                do row = 0, n_rows - 1
                    call partition( row, n_rows, side, rb, re )
                    rb = merge( rb - 1, rb, row == 0 )
                    re = merge( re + 1, re, row == n_rows - 1 )
                    do i = rb, re
                        read *, t_row(:)
                        do col = 0, n_cols - 1
                            call partition( col, n_cols, side, cb, ce )
                            dst = get_rank( row, col, n_rows, n_cols )
                            if (dst == root) then
                                mtx(i, cb - 1:ce + 1) = t_row(cb - 1:ce + 1)
                            else
                                call MPI_Send( t_row(cb - 1), ce - cb + 3, MPI_REAL, dst, 0, MPI_COMM_WORLD )
                            end if
                        end do
                    end do
                end do
                deallocate(t_row)
            end block
        else ! (my_rank /= root)
            rb = merge( ib - 1, ib, row == 0 )
            re = merge( ie + 1, ie, row == n_rows - 1 )
            do i = rb, re
                call MPI_Recv( mtx(i, jb - 1), 1, a_row, root, 0, MPI_COMM_WORLD, status )
            end do
        end if
    end subroutine read_temperature
    !---------------------------------------------------------------------------
    ! Homework:
    subroutine print_temperature( mtx, side )
        real, dimension(:, :), pointer, intent(in out) :: mtx
        integer,                        intent(in)     :: side
        ! TODO

        if (my_rank == root) then
            block
                real, dimension(:), allocatable :: t_row
                allocate(t_row(0:side + 1))
                ! TODO
                deallocate(t_row)
            end block
        else
            ! TODO
        end if
    end subroutine print_temperature
    !---------------------------------------------------------------------------
    subroutine print_blocks( mtx )
        real, dimension(:, :), pointer, intent(in out) :: mtx
        !
        integer :: rank, i

        do rank = 0, n_ranks - 1
            if (rank == my_rank) then
                print '(a, i0)', "Rank ", my_rank
                do i = lbound( mtx, dim=1 ), ubound( mtx, dim=1 )
                    print '(*(f5.0))', mtx(i, :)
                end do
            end if
            call MPI_Barrier( MPI_COMM_WORLD )
        end do
    end subroutine print_blocks
    !---------------------------------------------------------------------------
    ! Wait specified number of ms and then clear the terminal screen.
    subroutine wait_cls( ms )
       integer, intent(in) :: ms
       integer :: tick, tack
       real :: rate
       call system_clock( count=tick, count_rate=rate )
       do
           call system_clock( count=tack )
           if (real( tack - tick ) / rate >= ms * 1e-3) exit
       end do
       call MPI_Barrier( MPI_COMM_WORLD )
       ! Clear the terminal screen using the console escape code ^[2J.
       if (my_rank == root) print "(2a)", achar( 27 ), "[2J"
    end subroutine wait_cls
    !---------------------------------------------------------------------------
    subroutine next_iteration( old_mtx, new_mtx )
        real, dimension(:, :), pointer, intent(in)     :: old_mtx
        real, dimension(:, :), pointer, intent(in out) :: new_mtx
        !
        real, parameter :: alpha = 2.5e-4

        new_mtx(ib:ie, jb:je) = &
            (1.0 - 4.0 * alpha) *   old_mtx(ib    :ie,     jb    :je    ) &
                       + alpha  * ( old_mtx(ib - 1:ie - 1, jb    :je    ) &
                                  + old_mtx(ib + 1:ie + 1, jb    :je    ) &
                                  + old_mtx(ib    :ie,     jb - 1:je - 1) &
                                  + old_mtx(ib    :ie,     jb + 1:je + 1) )
    end subroutine next_iteration
    !---------------------------------------------------------------------------
    subroutine update_borders( old_mtx, new_mtx )
        real, dimension(:, :), pointer, intent(in)     :: old_mtx
        real, dimension(:, :), pointer, intent(in out) :: new_mtx
        
        ! Inner columns:
        new_mtx(ib:ie, jb - 1) = old_mtx(ib:ie, jb - 1)
        new_mtx(ib:ie, je + 1) = old_mtx(ib:ie, je + 1)
        ! Full rows:
        new_mtx(ib - 1, jb - 1:je + 1) = old_mtx(ib - 1, jb - 1:je + 1)
        new_mtx(ie + 1, jb - 1:je + 1) = old_mtx(ie + 1, jb - 1:je + 1)
    end subroutine update_borders
    !---------------------------------------------------------------------------
    ! 2D topology, forward problem: get the row and column coordinates in a 2D
    ! Cartesian topology for the given process rank.  The process rank might
    ! fall outside the legal range by mistake.  The Cartesian topology is
    ! column-major as in Fortran.
    subroutine get_coords( rank, n_rows, n_cols, row, col )
        integer, intent(in)    :: rank, n_rows, n_cols
        integer, intent(inout) :: row, col

        row = modulo(rank, n_rows)
        col = (rank - row) / n_rows
        if (0 <= col .and. col < n_cols) then
            return
        else
            print "(a, 2(i0, a))", "get_coords: rank ", rank, &
                " is outside the column range [0, ", n_cols, ")."
            call MPI_Abort( MPI_COMM_WORLD, MPI_ERR_TOPOLOGY )
        end if
    end subroutine get_coords
    !---------------------------------------------------------------------------
    ! 2D topology, reverse problem: get the process rank from the coordinates of
    ! its block in a 2D Cartesian topology.  The Cartesian topology is column-
    ! major as in Fortran.  The row and column can be oustide the legal range,
    ! in this case the process number is null.  The boundaries are not periodic
    ! to avoid communicating utmost ghost cells to neighbors to keep the
    ! boundary conditions constant.
    integer function get_rank( row, col, n_rows, n_cols )
        integer, intent(in) :: row, col, n_rows, n_cols

        if (      0 <= col .and. col < n_cols &
            .and. 0 <= row .and. row < n_rows) then
                get_rank = row + col * n_rows
        else
            get_rank = MPI_PROC_NULL
        end if
    end function get_rank
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
    subroutine synchronize_ghost( mtx )
        real, dimension(:, :), pointer, intent(in out) :: mtx
        
        ! Synchronize inner columns.
        call MPI_Sendrecv( mtx(ib, jb    ), 1, a_col, west_rank, 1, &
                           mtx(ib, je + 1), 1, a_col, east_rank, 1, MPI_COMM_WORLD, status )
        call MPI_Sendrecv( mtx(ib, je    ), 1, a_col, east_rank, 2, &
                           mtx(ib, jb - 1), 1, a_col, west_rank, 2, MPI_COMM_WORLD, status )
        ! Synchronize full rows.
        call MPI_Sendrecv( mtx(ib,     jb - 1), 1, a_row, north_rank, 3, &
                           mtx(ie + 1, jb - 1), 1, a_row, south_rank, 3, MPI_COMM_WORLD, status )
        call MPI_Sendrecv( mtx(ie,     jb - 1), 1, a_row, south_rank, 4, &
                           mtx(ib - 1, jb - 1), 1, a_row, north_rank, 4, MPI_COMM_WORLD, status )
    end subroutine synchronize_ghost
    !---------------------------------------------------------------------------
end program heat_parallel
