program test_struct2
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank
    integer(kind=MPI_ADDRESS_KIND) :: addrs(2), laddr, uaddr
    type(MPI_Datatype) :: a_struct

    type :: an_elem
        real :: x = 0.0
        character(len=1) :: ch = ''
    end type an_elem

    type(an_elem) :: elems(2)

    call MPI_Init()

    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    call MPI_Get_address( elems(1),     laddr    ) ! 0
    call MPI_Get_address( elems(1)%x,   addrs(1) ) ! 0
    call MPI_Get_address( elems(1)%ch,  addrs(2) ) ! 4
    call MPI_Get_address( elems(2),     uaddr    ) ! ?

    block
        integer :: c
        do c = 1, size( addrs )
            addrs(c) = MPI_Aint_diff( addrs(c), laddr )
        end do
        uaddr = MPI_Aint_diff( uaddr, laddr )
    end block
    print "(a, 2i3, a, i0)", "addrs(:) = ", addrs(:), ", extent(an_elem) = ", uaddr

    call MPI_Type_create_struct( 2, [1, 1], addrs, [MPI_REAL, MPI_CHARACTER], a_struct )
    call MPI_Type_get_extent( a_struct, laddr, uaddr )

    print "(a, i0)", "extent(a_struct) = ", MPI_Aint_diff( uaddr, laddr )

    call MPI_Finalize()
end program test_struct2
