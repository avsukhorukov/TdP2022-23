program pack_unpack
    use mpi_f08
    implicit none
    integer :: n_ranks, my_rank, first, last
    type(MPI_Status) :: status

    integer :: i = 0
    real :: x = 0.0
    character(len=6) :: name = ''

    integer, parameter     :: buf_len = 100
    character(len=buf_len) :: buf
    integer :: position

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    first = 0;  last  = n_ranks - 1

    if (my_rank == first) then
        i = 5;  x = -1.0;  name = 'test'

        position = 0
        call MPI_Pack( i,    1,           MPI_INTEGER,   buf, buf_len, position, MPI_COMM_WORLD )
        call MPI_Pack( x,    1,           MPI_REAL,      buf, buf_len, position, MPI_COMM_WORLD )
        call MPI_Pack( name, len( name ), MPI_CHARACTER, buf, buf_len, position, MPI_COMM_WORLD )

        call MPI_Send( buf, position, MPI_PACKED, last, 0, MPI_COMM_WORLD )
    else if (my_rank == last) then
        print "(2(a, i0), a, f5.1, 3a)", &
            "Rank ", my_rank, ": i=", i, ", x=", x, ", name='", name, "'."

        call MPI_Recv( buf, buf_len, MPI_PACKED, first, 0, MPI_COMM_WORLD , status )
        position = 0
        call MPI_Unpack( buf, buf_len, position, i,    1,         MPI_INTEGER,   MPI_COMM_WORLD )
        call MPI_Unpack( buf, buf_len, position, x,    1,         MPI_REAL,      MPI_COMM_WORLD )
        call MPI_Unpack( buf, buf_len, position, name, len(name), MPI_CHARACTER, MPI_COMM_WORLD )

        print "(2(a, i0), a, f5.1, 3a)", &
            "Rank ", my_rank, ": i=", i, ", x=", x, ", name='", name, "'."
    end if

    call MPI_Finalize()
end program pack_unpack
