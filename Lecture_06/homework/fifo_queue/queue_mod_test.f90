! (A. Stolyarov) Problem 2: code a program that
!
! 1) reads numbers from stdin until wrong input or EoF (press ctrl+d);
! 2) stores the entered numbers in a singly linked list by adding them to the
!    list's tail;
! 3) prints the stored numbers in the normal order;
! 4) destroys the list.
!
! For the normal order, new items must be added to the list's tail.  This is the
! put/get functionality of a FIFO queue.
!
! In this example we code everything in a separate module.  Compile both sources
! twice:
!
!   $ gfortran ... queue_mod.f90 queue_mod_test.f90 
!
program queue_mod_test
    use :: queue_mod
    implicit none
    integer :: iostatus, val
    type(a_sll_item), pointer :: head, tail

    call queue_init( head, tail )
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call queue_put( head, tail, val )   ! append to the tail
    end do

    call queue_display( head, tail )

    call queue_destroy( head, tail )
end program queue_mod_test