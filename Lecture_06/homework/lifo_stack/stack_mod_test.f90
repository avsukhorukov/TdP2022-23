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
! This version uses the `stack_mod.f90` module.  Compile:
!
!   $ gfortran ... stack_mod.f90 stack_mod_test.f90
!
program stack_mod_test
    use :: stack_mod
    implicit none
    type(a_sll_item), pointer :: head
    integer :: val, iostatus

    head => null()
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call stack_push( head, val )
        !call stack_insert( head, val )
    end do

    call stack_display( head )

    block
        type(a_sll_item), pointer :: temp

        print "(a)", "Enter values to search for (one per line):"
        do
            read(*, *, iostat=iostatus) val
            if (iostatus /= 0) exit
            temp => stack_search( head, val )
            if (associated( temp )) then
                !temp%val = temp%val * 2
                !call stack_display( head )
                print "(a, i0)", "temp%val = ", temp%val
            else
                print "(a, i0, a)", "Value ", val, " is not on the stack"
            end if
        end do
    end block

    ! ! Traversal using subroutine callbacks.
    ! call stack_traverse( head, sub_print, val )
    ! print "(a)", "null()"

    ! val = 0
    ! call stack_traverse( head, sub_sum, val )
    ! print "(a, i0)", "Sum over list is ", val

    ! val = -huge(0)
    ! call stack_traverse( head, sub_max, val )
    ! print "(a, i0)", "Max of list is ", val

    ! ! Traversal using function callbacks.
    ! print "(a, i0)", "Sum over list is ",     stack_l_reduce( head, func_sum,  0 )
    ! print "(a, i0)", "Product over list is ", stack_l_reduce( head, func_prod, 1 )
    ! print "(a, i0)", "Max over list is ",     stack_l_reduce( head, func_max,  -huge(0) )

    ! Deletion of specified items.
    print "(a)", "Values to delete (one per line):"
    do
        !if (.not.associated( head )) exit
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call stack_remove( head, val )
        call stack_display( head )
    end do

    call stack_destroy( head )

contains

    subroutine sub_print( n, data )
        integer, intent(in) :: n
        integer, intent(in out) :: data

        write(*, "(a, i0, a)", advance="no") "(", n, ")->"
    end subroutine sub_print

    subroutine sub_sum( n, data )
        integer, intent(in) :: n
        integer, intent(in out) :: data

        data = data + n
    end subroutine sub_sum

    subroutine sub_max( n, data )
        integer, intent(in) :: n
        integer, intent(in out) :: data

        data = max( data, n )
    end subroutine sub_max

    integer function func_sum( l, r )
        integer, intent(in) :: l, r
        func_sum = l + r
    end function func_sum

    integer function func_prod( l, r )
        integer, intent(in) :: l, r
        func_prod = l * r
    end function func_prod

    integer function func_max( l, r )
        integer, intent(in) :: l, r
        func_max = max( l, r )
    end function func_max

end program stack_mod_test