program type_indexed
    use :: mpi_f08
    use :: parallel_mod, only : serial_print
    implicit none
    integer :: n_ranks, my_rank, first, last
    type(MPI_Status) :: status
    integer, parameter :: side = 5
    integer :: a(side, side) = 0
    integer, dimension(side) :: block_lens, displacements
    integer :: i
    type(MPI_Datatype) :: a_ltriangle

    call MPI_Init()

    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    first = 0;  last = n_ranks - 1

    block_lens(:)    = [(side - i + 1,          i=1, side)]  ! [5, 4, 3, 2, 1]
    displacements(:) = [((side + 1) * (i - 1),  i=1, side)]  ! [0, 6, 12, 18, 24], main diagonal

    call MPI_Type_indexed( side, block_lens, displacements, MPI_INTEGER, a_ltriangle )
    call MPI_Type_commit( a_ltriangle )

    if (my_rank == first) then
        a(:, :) = reshape( [(i,  i=1, side * side)], shape=[side, side] )
        call MPI_Send( a, 1, a_ltriangle, last, 0, MPI_COMM_WORLD )
    else if (my_rank == last) then
        call MPI_Recv( a, 1, a_ltriangle, first, 0, MPI_COMM_WORLD, status )
    end if

    call serial_print( a, my_rank, n_ranks, MPI_COMM_WORLD )

    call MPI_Type_free( a_ltriangle )
    call MPI_Finalize()
end program type_indexed
