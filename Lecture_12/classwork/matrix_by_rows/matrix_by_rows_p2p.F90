! Write a program that distributes an identity matrix over the processes.  The
! dimension and the number of processes are given.  Distribute by rows (Fortran).
! The program reads the matrix size S, takes the number of processes N, and
! calculates how many columns (with remainder) must take every process.
! You can either operate with local or global indices on each process.  At the
! end, rank 0 collects parts of the matrix from other ranks and prints the
! entire thing.
!
! Compile and run:
!
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!   parallel_mod.f90 matrix_by_rows_p2p.F90
! $ mpirun -np 3 --oversubscribe ./a.out # enter 10
!
program matrix_by_rows_p2p
    use :: mpi_f08
    use :: parallel_mod, only : get_open_rank
    implicit none
    integer              :: s       ! matrix size
    integer, allocatable :: e(:, :) ! matrix itself
    integer :: my_rank, n_ranks, upper_rank, lower_rank
    integer :: remainder, quotient, n_rows, i, ib, ie
    type(MPI_Status) :: status
    type(MPI_Datatype) :: a_row

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    if (my_rank == 0) then
        print "(a)", "Enter the matrix size S:"
        read *, s
        block
            integer :: rank
            do rank = 1, n_ranks - 1
                call MPI_Send( s, 1, MPI_INTEGER, rank, 0, MPI_COMM_WORLD )
            end do
        end block
    else
        call MPI_Recv( s, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status )
    end if

    remainder = modulo( s, n_ranks )
    quotient  = (s - remainder) / n_ranks
    ! If E(:, :) is not evenly divisble, then first R processes have one extra row.
    ! if (my_rank < remainder) then
    !     n_rows = quotient + 1
    ! else
    !     n_rows = quotient
    ! end if
    n_rows = quotient + merge( 1, 0, my_rank < remainder )

!#if 0
    ! Inefficient method with three branches:
    if (my_rank == 0) then
       i = 1
       ib = i
       ie = i + n_rows - 1
       i = ie + 1
       call MPI_Send( i, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD )
    else if (0 < my_rank .and. my_rank < n_ranks - 1) then
       call MPI_Recv( i, 1, MPI_INTEGER, my_rank - 1, 0, MPI_COMM_WORLD, status )
       ib = i
       ie = i + n_rows - 1
       i = ie + 1
       call MPI_Send( i, 1, MPI_INTEGER, my_rank + 1, 0, MPI_COMM_WORLD )
    else ! (my_rank == n_ranks - 1)
       call MPI_Recv( i, 1, MPI_INTEGER, my_rank - 1, 0, MPI_COMM_WORLD, status )
       ib = i
       ie = i + n_rows - 1
    end if
!#endif

    ! More efficient method using MPI_PROC_NULL to suppress unnecessary calls.
!#if 0
    upper_rank = my_rank - 1
    lower_rank = my_rank + 1
    if (upper_rank == -1) upper_rank = MPI_PROC_NULL
    if (lower_rank == n_ranks) lower_rank = MPI_PROC_NULL
!#endif
    upper_rank = get_open_rank( my_rank - 1, n_ranks )
    lower_rank = get_open_rank( my_rank + 1, n_ranks )

    i = 1
    call MPI_Recv( i, 1, MPI_INTEGER, upper_rank, 0, MPI_COMM_WORLD, status )
    ib = i
    ie = i + n_rows - 1
    i = ie + 1
    call MPI_Send( i, 1, MPI_INTEGER, lower_rank, 0, MPI_COMM_WORLD )

    ! Initialize the local matrix.
    allocate(e(ib:ie, s), source=0)
    do i = ib, ie
        e(i, i) = 1
    end do

    ! Define a datatype for a full matrix row and resize it to the extent of
    ! one matrix element of type integer.
    block
        type(MPI_Datatype) :: a_tmp_row
        integer(kind=MPI_ADDRESS_KIND) :: lb, extent

        call MPI_Type_get_extent( MPI_INTEGER, lb, extent )
        call MPI_Type_vector( s, 1, n_rows, MPI_INTEGER, a_tmp_row )
        call MPI_Type_create_resized( a_tmp_row, lb, extent, a_row )
        call MPI_Type_commit( a_row )
    end block

#if 0
    if (my_rank == 0) then
        ! Rank 0 prints its own matrix,
        print "(a, i2)", "Rank ", my_rank
        do i = ib, ie ! 1, n_rows
            print "(*(i2))", e(i, :)
        end do
        ! then receives rows from the other processes and prints them.
        block
            integer :: rank
            do rank = 1, n_ranks - 1
                call MPI_Recv( e(1, 1), n_rows, a_row, rank, 0, MPI_COMM_WORLD, status )
                print "(a, i2)", "Rank ", rank
                do i = 1, quotient + merge(1, 0, rank < remainder)
                    print "(*(i2))", e(i, :)
                end do
            end do
        end block
    else
        call MPI_Send( e(ib, 1), n_rows, a_row, 0, 0, MPI_COMM_WORLD )
    end if
#endif
    if (my_rank == 0) then
        block
            integer :: rank
            do rank = 0, n_ranks - 1
                if (rank /= 0) &
                    call MPI_Recv( e(1, 1), n_rows, a_row, rank, 0, MPI_COMM_WORLD, status )
                print "(a, i2)", "Rank ", rank
                do i = 1, quotient + merge(1, 0, rank < remainder)
                    print "(*(i2))", e(i, :)
                end do
            end do
        end block
    else
        call MPI_Send( e(ib, 1), n_rows, a_row, 0, 0, MPI_COMM_WORLD )
    end if

    call MPI_Type_free( a_row )
    deallocate(e)

    call MPI_Finalize()
end program matrix_by_rows_p2p
