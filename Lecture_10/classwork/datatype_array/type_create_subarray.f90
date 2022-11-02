program type_create_subarray
    use :: mpi_f08
    use :: parallel_mod, only : serial_print
    implicit none
    integer :: n_ranks, my_rank, first, last
    type(MPI_Status) :: status
    integer, parameter :: side = 5
    integer :: a(side, side) = 0
    integer :: i
    type(MPI_Datatype) :: a_subarray

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    first = 0;  last = n_ranks - 1

    call MPI_Type_create_subarray( rank( a ), shape( a ), [3, 3], [1, 1], &
                                   MPI_ORDER_FORTRAN, MPI_INTEGER, a_subarray )
    call MPI_Type_commit( a_subarray )

    if (my_rank == first) then
        a(:, :) = reshape( [(i, i = 1, side * side)], shape=[side, side] )
        call MPI_Send( a, 1, a_subarray, last, 0, MPI_COMM_WORLD )
    else if (my_rank == last) then
        call MPI_Recv( a, 1, a_subarray, first, 0, MPI_COMM_WORLD, status )
    end if

    call serial_print( a, my_rank, n_ranks, MPI_COMM_WORLD )

    call MPI_Type_free( a_subarray )
    call MPI_Finalize()
end program type_create_subarray