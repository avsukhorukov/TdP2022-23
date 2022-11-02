! Send two unique messages from all non-zero ranks to rank 0.  The first message
! should be the same as in the previous example (send_string.f90), the other one
! should be a bit different.  Send them using tag=0 and tag=1.
!
! At rank 0 receive and print the messages with tag=0 one-by-one from non-zero
! ranks.  Receive and print the other group of messages with tag=1 using
! source=MPI_ANY_SOURCE in random order.  Nevertheless, they will mostly appear
! sorted.
!
! Compile and run:
!
!   $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace \
!     send_string_tags.f90
!   $ mpirun -np 4 --oversubscribe ./a.out
!
! Observe that both groups are printed in the correct order altough it is not
! expected for the second group.
!
! On receive, use source=MPI_ANY_SOURCE for the 1st group and demonstrate that
! now messages are printed in random order.
!
! This example requires many low-level details of the MPI library to be
! explained:
! 1) the envelope is matched on source, dest, tag, and comm values.
! 2) if the envelopes are identical then the library receives messages in the
!    sent order for the case of point-to-point communication.
! 3) for small messages the buffered communication mode is used, it is fast
!    (takes mks) and no waiting is needed on send.  Messages arrive faster than
!    they are printed (I/O is an OS call, takes ms), so they appear ordered by
!    following prints of the first group.
! 4) for big messages the synchronous (rendezvous) communication mode is used,
!    it is slow as is waiting for a handshake, so messages might arrive at
!    random times depending on which processes communicate, and they appear
!    shuffled.
! 5) On systems with little number of cores (when we use the --oversubscribe)
!    there is no real parallelizm and the communication might be serialized in
!    the order of PIDs of the processes due to the OS multitasking and
!    scheduling.
program send_string_tags
    use mpi_f08
    implicit none

    integer :: my_rank, n_ranks, src
    integer, parameter      :: msg_size = 40
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