! Compile together with `parallel_mod.f90` and run with 2 or more processes:
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     parallel_mod.f90 sendrecv.f90
!   $ mpirun -np 2 --ovsersubscribe ./a.out
!
program type_vector
    use :: mpi_f08
    use :: parallel_mod, only : serial_print
    implicit none
    integer :: n_ranks, my_rank, first, last
    type(MPI_Status) :: status
    integer, parameter :: side = 5
    integer :: a(side, side)
    integer :: i
    type(MPI_Datatype) :: a_row
    type(MPI_Datatype) :: a_col

    call MPI_Init()

    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    first = 0;  last = n_ranks - 1

    call MPI_Type_vector( side, 1, side, MPI_INTEGER, a_row )
    call MPI_Type_commit( a_row )

    call MPI_Type_contiguous( side, MPI_INTEGER, a_col )
    call MPI_Type_commit( a_col )

    a(:, :) = 0

    if (my_rank == first) then
        a(:, :) = reshape( [(i,  i=1, side * side)], shape=[side, side] )
        call MPI_Send( a(1, 1), 1, a_row, last, 0, MPI_COMM_WORLD )
    else if (my_rank == last) then
        call MPI_Recv( a(1, 1), 1, a_col, first, 0, MPI_COMM_WORLD, status )
    end if

    call serial_print( a, my_rank, n_ranks, MPI_COMM_WORLD )

    call MPI_Type_free( a_row )
    call MPI_Type_free( a_col )
    call MPI_Finalize()
end program type_vector
