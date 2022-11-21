! Solution 1
!
! The array of bodies is virtually distributed between all processes.  It is not
! divided between them, all processes create the array of all bodies.  But each
! process updates coordinates, velocities, and accelerations only in its own
! range of bodies.  At the end, new coordinates are gathered to all from
! different ranks.  This solution is the same as the exact one.
!
module parallel_mod
    use :: mpi_f08
    implicit none

    ! Process ranks:
    integer, parameter :: root_rank = 0
    integer :: n_ranks, my_rank

    ! The local range of bodies is first:last, and count = first - last + 1.
    integer :: first, last, count

    ! The arrays of sizes and displacements for vector collectives.  Must be
    ! known on all ranks.
    integer, dimension(:), allocatable :: counts, disps

    ! Datatypes for the entire body and for the coordinate component only.
    type(MPI_Datatype) :: a_body_datatype, a_body_r_datatype

contains

    subroutine init_parallel()
        call MPI_Init()
        call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
        call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    end subroutine init_parallel

    subroutine finish_parallel()
        if (allocated(counts)) deallocate(counts)
        if (allocated(disps)) deallocate(disps)
        call MPI_Finalize()
    end subroutine finish_parallel

    subroutine partition( size )
        integer, intent(in) :: size
        integer :: remainder, quotient

        remainder = modulo( size, n_ranks )
        quotient = (size - remainder) / n_ranks
        first = 1 + quotient * (my_rank    ) + min( remainder, my_rank     )
        last  =     quotient * (my_rank + 1) + min( remainder, my_rank + 1 )
        count = last - first + 1
        allocate(counts(0:n_ranks - 1))
        allocate(disps(0:n_ranks - 1))
        call MPI_Allgather( count,     1, MPI_INTEGER, counts, 1, MPI_INTEGER, MPI_COMM_WORLD )
        call MPI_Allgather( first - 1, 1, MPI_INTEGER, disps,  1, MPI_INTEGER, MPI_COMM_WORLD )
    end subroutine partition

end module parallel_mod