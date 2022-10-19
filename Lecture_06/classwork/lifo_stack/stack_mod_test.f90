! Compile either using -DNONRECURSIVE or -DRECURSIVE.  For example,
!
!   $ gfortran -g -O0 -Wall -Wextra -Wpedantic -fbacktrace -fcheck=all \
!              -DNONRECURSIVE stack_mod.F90 stack_mod_test.f90 
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
                temp%val = temp%val * 2
                call stack_display( head )
            else
                print "(a, i0, a)", "Value ", val, " is not on the stack"
            end if
        end do
    end block

    ! do
    !     if (.not.associated( head )) exit
    !     val = stack_pop( head )
    ! end do
    call stack_destroy( head )
end program stack_mod_test