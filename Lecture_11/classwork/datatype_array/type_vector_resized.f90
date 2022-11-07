program type_vector_resized
    use mpi_f08
    use :: parallel_mod, only : serial_print
    implicit none
    integer, parameter :: side = 5
    integer            :: a(side, side)
    integer :: n_ranks, my_rank, first, last, i
    type(MPI_Status) :: status
    type(MPI_Datatype) :: a_row, a_resized_row
    integer(kind=MPI_ADDRESS_KIND) :: lb, ub

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    first = 0;  last  = n_ranks - 1

    call MPI_Type_vector( side, 1, side, MPI_INTEGER, a_row )
    ! call MPI_Type_commit( a_row )
    call MPI_Type_get_extent( MPI_INTEGER, lb, ub )
    call MPI_Type_create_resized( a_row, lb, ub, a_resized_row )
    call MPI_Type_commit( a_resized_row )

    a(:, :) = 0
    if (my_rank == first) then
        a(:, :) = reshape( [(i,  i=1, side * side)], shape=[side, side] )
        call MPI_Send( a(1, 1), 3, a_resized_row, last, 0, MPI_COMM_WORLD )
    else if (my_rank == last) then
        call MPI_Recv( a(1, 1), 5, a_resized_row, first, 0, MPI_COMM_WORLD, status )
    end if

    call serial_print( a, my_rank, n_ranks, MPI_COMM_WORLD )

    ! call MPI_Type_free( a_row )
    call MPI_Type_free( a_resized_row )
    call MPI_Finalize()
end program type_vector_resized
