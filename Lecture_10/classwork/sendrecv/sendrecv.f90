! Demonstrate first what happens if you just use first and last: rank 0
! deadlocks, draw on the blackboard the diagram, as rank 1 sends to itself, but
! rank 0 tries to recieve from itself.
!
! Compile:
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     sendrecv.f90
!
! Run with 3 or more processes:
!
!   $ mpirun -np 3 --ovsersubscribe ./a.out
!
program deadlock
    use mpi_f08
    implicit none
    integer, parameter :: n = 5 ! array size and format repetition
    integer :: my_rank, n_ranks, prev, next
    integer, allocatable :: a(:), b(:)
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    prev = modulo( my_rank - 1, n_ranks )
    next = modulo( my_rank + 1, n_ranks )

    allocate(a(n), source=my_rank) ! all values are my_rank
    allocate(b(n), source=-1)      ! all values are -1

    print "(a, i2, 2(a, 5(1x, i2)), a)", &
        "before at rank ", my_rank, ", a = [", a(:), "], b = [", b(:), "]"

    call MPI_Sendrecv( &
        sendbuf=a, sendcount=n, sendtype=MPI_INTEGER, dest=next,   sendtag=0, &
        recvbuf=b, recvcount=n, recvtype=MPI_INTEGER, source=prev, recvtag=0, &
        comm=MPI_COMM_WORLD, status=status )

    print "(a, i2, 2(a, 5(1x, i2)), a)", &
        "after  at rank ", my_rank, ", a = [", a(:), "], b = [", b(:), "]"

    deallocate(a)
    deallocate(b)

    call MPI_Finalize()
end program deadlock