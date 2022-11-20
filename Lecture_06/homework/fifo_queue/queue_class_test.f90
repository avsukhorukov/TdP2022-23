! (A. Stolyarov) Problem 2: code a program that does the following:
!
! 1) reads numbers from stdin until wrong input or EoF (ctrl+d is pressed);
! 2) stores the numbers in a singly linked list by adding them to the list's
!    tail;
! 3) prints the stored numbers in the normal order;
! 4) destroys the list.
!
! For the normal order, the new nodes must be added to the list's tail.  This
! is the put/get functionality of a FIFO queue.
!
! Compile this twice:
!
!   $ gfortran ... queue_class_test.f90 queue_class.f90
!
program queue_class_test
    use :: queue_class, only: a_queue
    implicit none
    type(a_queue) :: queue
    integer :: iostatus, val

    call queue%init()
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call queue%put( val )   ! append to the tail
    end do

    call queue%display()

    ! TODO: write a proper destructor instead of this.
    do
        if (queue%is_empty()) exit
        val = queue%get()
    end do
end program queue_class_test