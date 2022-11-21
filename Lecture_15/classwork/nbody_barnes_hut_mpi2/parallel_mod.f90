! Solution 2
!
! The tree is split in four parts at level 1 between the four processes.  The
! array of bodies is the same of the full size for all processes.
!
module parallel_mod
    use :: mpi_f08
    implicit none

    ! Process ranks:
    integer, parameter :: root_rank = 0
    integer :: n_ranks, my_rank

    ! A Cartesian topology of the problem.
    integer :: n_rows = 2, n_cols = 2
    integer, dimension(2) :: topology ! [row, col]

    ! A datatypes for the entire body.
    type(MPI_Datatype) :: a_body_datatype

    ! A reduction callback that adds up two accelerations of a given body.
    type(MPI_Op) :: sum_body_a_op

contains

    subroutine init_parallel()
        call MPI_Init()
        call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
        call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
        call create_topology()
    end subroutine init_parallel

    subroutine finish_parallel()
        call MPI_Finalize()
    end subroutine finish_parallel

    ! Create a 2D Cartesian topology with 2x2 ranks.  The Fortran order of
    ! blocks is assumed: the topological row is fast, the column is slow.
    subroutine create_topology()
        integer :: tmp, r, c

        if (n_rows * n_cols /= n_ranks) then
            if (my_rank == root_rank) print "(a)", "Run with `-np 4` only."
            call MPI_Abort( MPI_COMM_WORLD, MPI_ERR_TOPOLOGY )
        end if
        tmp = my_rank               ! = my_rank = r + c * n_rows
        r   = modulo( tmp, n_rows )
        tmp = (tmp - r) / n_rows    ! tmp = c
        c   = modulo( tmp, n_cols )
        topology(:) = [r, c]
    end subroutine create_topology

end module parallel_mod