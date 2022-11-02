program test_struct
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank
    integer(kind=MPI_ADDRESS_KIND) :: addrs(5), laddr, uaddr
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

    call MPI_Get_address( elems(1),     addrs(1) ) ! 0
    call MPI_Get_address( elems(1)%i,   addrs(2) ) ! 0
    call MPI_Get_address( elems(1)%r,   addrs(3) ) ! 4
    call MPI_Get_address( elems(1)%str, addrs(4) ) ! 8
    call MPI_Get_address( elems(2),     addrs(5) ) ! 16

    block
        integer :: c
        laddr = addrs(1)
        do c = 1, size( addrs )
            addrs(c) = MPI_Aint_diff( addrs(c), laddr )
        end do
    end block
    print "(a, 5i3)", "addrs(:) = ", addrs(:)

    call MPI_Type_create_struct( 3, [1, 1, 6], addrs, &
                                 [MPI_INTEGER, MPI_REAL, MPI_CHARACTER], a_struct )
    call MPI_Type_get_extent( a_struct, laddr, uaddr )
    ! call MPI_Type_commit( a_struct )
    print "(a, i0)", "extent(a_struct) = ", MPI_Aint_diff( uaddr, laddr )

    ! call MPI_Type_free( a_struct )
    call MPI_Finalize()
end program test_struct
