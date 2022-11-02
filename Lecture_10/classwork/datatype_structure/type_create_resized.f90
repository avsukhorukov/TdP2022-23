program type_create_resized
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank, first, last
    type(MPI_Status) :: status
    integer(kind=MPI_ADDRESS_KIND) :: addrs(4), laddr, uaddr
    type(MPI_Datatype) :: a_struct, a_struct_arr

    type :: a_body
        real :: m    = 0.0
        real :: r(2) = [0.0, 0.0]
        real :: v(2) = [0.0, 0.0]
        real :: a(2) = [0.0, 0.0]
    end type a_body

    type(a_body), allocatable :: bodies(:)

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    allocate(bodies(2))

    call MPI_Get_address( bodies(1), laddr )
    call MPI_Get_address( bodies(1)%m, addrs(1) )
    call MPI_Get_address( bodies(1)%r, addrs(2) )
    call MPI_Get_address( bodies(1)%v, addrs(3) )
    call MPI_Get_address( bodies(1)%a, addrs(4) )
    call MPI_Get_address( bodies(2), uaddr )
    block
        integer :: c
        do c = 1, size( addrs )
            addrs(c) = MPI_Aint_diff( addrs(c), laddr ) ! addrs(:) = addrs(:) - laddr
        end do
        uaddr = MPI_Aint_diff( uaddr, laddr ) ! ub = extent of a_body
        laddr = MPI_Aint_diff( laddr, laddr ) ! lb = 0
    end block
    print "(a, 4i3, a, i0)", "addrs(1:4) = ", addrs, ", extent(a_body) = ", uaddr
    ! stop

    call MPI_Type_create_struct( 4, [1, 2, 2, 2], addrs, [MPI_REAL, MPI_REAL, MPI_REAL, MPI_REAL], a_struct )
    call MPI_Type_get_extent( a_struct, laddr, uaddr )
    ! or use laddr uaddr from the previous measure of two array elements.
    call MPI_Type_create_resized( a_struct, laddr, uaddr, a_struct_arr )
    call MPI_Type_commit( a_struct_arr )

    first = 0;  last = n_ranks - 1

    if (my_rank == first) then
        bodies(1) = a_body(1.0, [1.0, 2.0], [0.1, 0.2], [9.0, 8.0])
        bodies(2) = a_body(2.0, [2.0, 4.0], [0.2, 0.4], [6.0, 5.0])
        call MPI_Send( bodies(1), 2, a_struct_arr, last, 0, MPI_COMM_WORLD )
    else if (my_rank == last) then
        print '(a, i0, a, 2(7f4.1, a))', "Rank ", my_rank, &
            ": bodies = [", bodies(1), ", ", bodies(2), "]"

        call MPI_Recv( bodies(1), 2, a_struct_arr, first, 0, MPI_COMM_WORLD, status )

        print '(a, i0, a, 2(7f4.1, a))', "Rank ", my_rank, &
            ": bodies = [", bodies(1), ", ", bodies(2), "]"
    end if

    deallocate(bodies)
    call MPI_Type_free( a_struct_arr )
    call MPI_Finalize()
end program type_create_resized
