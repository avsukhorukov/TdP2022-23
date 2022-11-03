program get_address
    use mpi_f08
    implicit none
    integer :: i, j
    integer(kind=MPI_ADDRESS_KIND) :: addr_i, addr_j, lb, extent

    call MPI_Init()

    call MPI_Get_address( i, addr_i )   ! Variables i and j are in the local stack frame,
    call MPI_Get_address( j, addr_j )   !   therefore addr(i) > addr(j)
    call MPI_Type_get_extent( MPI_INTEGER, lb, extent )
    
    print "(z0)", MPI_Aint_diff( addr_i, addr_j ) ! addr(i) - addr(j) = 4
    print "(z0)", addr_i
    print "(z0)", MPI_Aint_add( addr_j, extent )  ! addr(j) + extent(integer) = addr(i)

    call MPI_Finalize()
end program get_address