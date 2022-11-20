! (A. Stolyarov) Problem 1: code a program that does the following:
!
! 1) reads numbers from stdin until wrong input or EoF (ctrl+d) using the iostat
!    flag;
! 2) stores the numbers in a singly linked list by pushing new items to the
!    list's head;
! 3) prints the entered numbers in reversed order by walking through the list
!    from head to tail;
! 4) destroys the list.
!
! The reversed order is obtained by adding new items to the head.  This is the
! push/pop functionality of a LIFO stack.
!
! This version uses the `stack_class.f90` module to emulate a stack class.
! Compile it twice:
!
!   $ gfortran ... stack_class.f90 stack_class_test.f90
!
program stack_class_test
    use :: stack_class, only: a_stack
    implicit none
    type(a_stack) :: stack
    integer :: iostatus, val

    call stack%init()
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call stack%push( val ) ! prepend to the head
    end do

    call stack%display()

    do
        if (stack%is_empty()) exit
        val = stack%pop()
    end do
    ! call stack%destroy()
end program stack_class_test