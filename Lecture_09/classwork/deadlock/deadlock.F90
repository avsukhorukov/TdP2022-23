! 1) Run with `-np 2`.
! 2) Show Case I for n = 40 and 4040
! 3) Show Case II for n = 40 and 4040.  In the last, demonstrate the deadlock by
!    attaching gdb to the two running processes.
! 4) Show Case III for n = 40.  Total deadlock.
!
! When running into a deadlock, demonstrate that the program has stopped at the
! same lines with MPI_Send.  Use two gdb sessions in separate terminals and
! attach them to the two running processes.  The backtrace will show that they
! are waiting inside the libopenmpi library.  Choose the right frame and go back
! to the program.
program deadlock
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, first, last
    integer, parameter   :: n = 40000
    integer, allocatable :: a(:), b(:)
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    first = 0;  last = n_ranks - 1

    allocate(a(n), source=my_rank)
    allocate(b(n), source=-1)

    ! No deadlock.
    if (my_rank == first) then
        call MPI_Send( a, n, MPI_INTEGER, last, 0, MPI_COMM_WORLD )
        call MPI_Recv( b, n, MPI_INTEGER, last, 0, MPI_COMM_WORLD, status )
        print '(a)', "Case I:   first rank done"
    else if (my_rank == last) then
        call MPI_Recv( b, n, MPI_INTEGER, first, 0, MPI_COMM_WORLD, status )
        call MPI_Send( a, n, MPI_INTEGER, first, 0, MPI_COMM_WORLD )
        print '(a)', "Case I:   last rank done"
    end if

    ! Exchange that relies on buffering.
    ! Deadlock at N > 1010 (4040 b).
    if (my_rank == first) then
        call MPI_Send( a, n, MPI_INTEGER, last, 0, MPI_COMM_WORLD )
        call MPI_Recv( b, n, MPI_INTEGER, last, 0, MPI_COMM_WORLD, status )
        print '(a)', "Case II:  first rank done"
    else if (my_rank == last) then
        call MPI_Send( a, n, MPI_INTEGER, first, 0, MPI_COMM_WORLD )
        call MPI_Recv( b, n, MPI_INTEGER, first, 0, MPI_COMM_WORLD, status )
        print '(a)', "Case II:  last rank done"
    end if

    ! Total deadlock.
    if (my_rank == first) then
        call MPI_Recv( b, n, MPI_INTEGER, last, 0, MPI_COMM_WORLD, status )
        call MPI_Send( a, n, MPI_INTEGER, last, 0, MPI_COMM_WORLD )
        print '(a)', "Case III: first rank done"
    else if (my_rank == last) then
        call MPI_Recv( b, n, MPI_INTEGER, first, 0, MPI_COMM_WORLD, status )
        call MPI_Send( a, n, MPI_INTEGER, first, 0, MPI_COMM_WORLD )
        print '(a)', "Case III: last rank done"
    end if
#if 0
#endif

    deallocate(a)
    deallocate(b)
    call MPI_Finalize()
end program deadlock