! Factorial, calculated in a 1D ring topology.
!
! Note: I've modified this problem from the old Ángel's assignments.  Now all
! ranks must multiply so if 0 reads 1, then last rank n - 1 will send back to
! rank 0 exactly the number of n!.
!
! 1) Rank 0 reads a number.
! 2) Each rank r sends to rank r + 1 the received number multiplied by r + 1,
!    except the last process with rank = n - 1.
! 3) Last rank n - 1 sends the result back to rank 0, which prints it.
!
! Compile and run:
! $ mpifort -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace ring_factorial.f90
! $ mpirun -np 6 --oversubscribe ./a.out
program ring_factorial
    use mpi_f08
    implicit none
    integer :: my_rank, n_ranks, next, prev
    integer :: factorial
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )

    ! Create a 1D ring topology.
    next = get_rank( my_rank - 1, n_ranks )
    prev = get_rank( my_rank + 1, n_ranks )

    ! Read the number at rank 0 or receive it at the other ranks.
    if (my_rank == 0) then
        read *, factorial
    else
        call MPI_Recv( factorial, 1, MPI_INTEGER, next, 0, comm, status )
    end if

    ! All ranks multiply the number.
    factorial = factorial * (my_rank + 1)

    ! Send the number to the next rank in the ring.
    call MPI_Send( factorial, 1, MPI_INTEGER, prev, 0, comm )

    ! Now rank 0 receives the result and prints it.
    if (my_rank == 0) then
        call MPI_Recv( factorial, 1, MPI_INTEGER, next, 0, comm, status )
        print "(i0)", factorial
    end if

    call MPI_Finalize()

contains

    ! Use the modulo division to get the corresponding rank in the ring topology
    ! with `n` processes.
    integer function get_rank( rnk, n )
        integer, intent(in) :: rnk, n
        get_rank = modulo( rnk, n )
    end function get_rank

end program ring_factorial
