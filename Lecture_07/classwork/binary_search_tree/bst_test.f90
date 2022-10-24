program bst_test
    use :: bst_mod
    implicit none
    integer :: iostatus, val
    type(a_bst_node), pointer :: root

    root => null()
    do
        read(*, *, iostat=iostatus) val
        if (iostatus /= 0) exit
        call bst_insert( root, val )
    end do
    print *
    call bst_display( root )

    block
        type(a_bst_node), pointer :: temp
        print "(a)", "Enter values to search for (one per line):"
        do
            read(*, *, iostat=iostatus) val
            if (iostatus /= 0) exit
            temp => bst_search( root, val )
            if (associated( temp )) then
                print "(a, i0)", "temp%val = ", temp%val
            else
                print "(a, i0, a)", "Value ", val, " is not in the tree"
            end if
        end do
    end block

    block
        type(a_bst_node), pointer :: root, temp

        temp => bst_search( root, 6 )
        print "(a, i0)", "Full sum = ", bst_sum( temp )
        temp => bst_search( root, 15 )
        print "(a, i0)", "Partial sum at node 15 = ", bst_sum( temp )
        temp => bst_search( root, 9 )
        print "(a, i0)", "Partial sum at non-existing node = ", bst_sum( temp )
    end block

    call bst_destroy( root )
end program bst_test