program test_struct1
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank
    integer(kind=MPI_ADDRESS_KIND) :: addrs(3), laddr, uaddr
    type(MPI_Datatype) :: a_struct

    type :: an_elem
        integer          :: i
        real             :: r
        character(len=6) :: str
    end type an_elem

    type(an_elem) :: elems(2)

    call MPI_Init()

    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    call MPI_Get_address( elems(1),     laddr    ) ! 0
    call MPI_Get_address( elems(1)%i,   addrs(1) ) ! 0
    call MPI_Get_address( elems(1)%r,   addrs(2) ) ! 4
    call MPI_Get_address( elems(1)%str, addrs(3) ) ! 8
    call MPI_Get_address( elems(2),     uaddr    ) ! 16
    block
        integer :: c
        do c = 1, size( addrs )
            addrs(c) = MPI_Aint_diff( addrs(c), laddr )
        end do
        uaddr = MPI_Aint_diff( uaddr, laddr ) ! ub = extent of an_elem
        laddr = MPI_Aint_diff( laddr, laddr ) ! lb = 0
    end block
    print "(a, 3i3, a, i0)", "addrs(:) = ", addrs(:), ", extent(an_elem) = ", uaddr

    call MPI_Type_create_struct( 3, [1, 1, 6], addrs, &
                                 [MPI_INTEGER, MPI_REAL, MPI_CHARACTER], a_struct )
    call MPI_Type_get_extent( a_struct, laddr, uaddr )
    ! call MPI_Type_commit( a_struct )
    print "(a, i0)", "extent(a_struct) = ", uaddr

    ! call MPI_Type_free( a_struct )
    call MPI_Finalize()
end program test_struct1
