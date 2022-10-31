!
! Send a two unique messages from all non-zero ranks to rank 0.  The first
! message should be the same as in the previous example (send_string.f90), the
! other one should be a bit different.  Send them using tag=0 and tag=1.
!
! At rank 0 receive messages sent with the default tag, one-by-one from all
! non-zero ranks and print them.  Now receive the other group of messages with
! the different tag using source=MPI_ANY_SOURCE in random order and print them
! too.  They will mostly appear sorted.
!
! Compile and run:
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     send_string_tags.f90
!   $ mpirun -np 4 --oversubscribe ./a.out
!
! Observe that both groups are printed in the correct order.
!
! On receive, use source=MPI_ANY_SOURCE for the 1st group and demonstrate that
! messages are printed in random order.
!
! TODO: this example requires too many technical details to be explaiend: what
! and how is matched in the envelope, what's the difference between the buffered
! and synchronous mode (the buffered mode uses the same internal 4 KiB buffer
! and orders messages on arrival before flushing the stdout stream, while the
! synchronous mode receives messages completely shuffled in time), and why on
! systems that emulate many processors, what we have with --ovsersubscribe,
! messages are serialized in the order of PIDs of their processes because the
! operative system emulates multitasking and schedules their execution in a
! simple serial way.
program send_string_tags
    use mpi_f08
    implicit none

    integer :: my_rank, n_ranks, src
    integer, parameter      :: msg_size = 4000
    character(len=msg_size) :: message
    type(MPI_Status) :: status

    call MPI_Init()
    call MPI_Comm_size( MPI_COMM_WORLD, n_ranks )
    call MPI_Comm_rank( MPI_COMM_WORLD, my_rank )

    if (my_rank /= 0) then

        write(message, "(a, i0)") "Greetings from rank ", my_rank
        call MPI_Send( message, msg_size, MPI_CHARACTER, 0, 0, MPI_COMM_WORLD )

        write(message, "(a, i0)") "Another greetings from rank ", my_rank
        call MPI_Send( message, msg_size, MPI_CHARACTER, 0, 1, MPI_COMM_WORLD )

    else ! (my_rank == 0)

        do src = 1, n_ranks - 1
            call MPI_Recv( message, msg_size, MPI_CHARACTER, src, 0, MPI_COMM_WORLD, status )
            print "(3a)", "|", trim(message), "|"
        end do
        print *
        do src = 1, n_ranks - 1
            call MPI_Recv( message, msg_size, MPI_CHARACTER, MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, status )
            print "(3a)", "|", trim(message), "|"
        end do

    end if

    call MPI_Finalize()
end program send_string_tags