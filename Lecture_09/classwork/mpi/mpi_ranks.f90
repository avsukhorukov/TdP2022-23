program mpi_ranks
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    print "(2(a, i0))", "Rank ", my_rank, " of ", n_ranks
    call MPI_Finalize()
end program mpi_ranks