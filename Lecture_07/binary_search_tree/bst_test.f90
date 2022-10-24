! Compile either using -DNONRECURSIVE or -DRECURSIVE.  For example,
!
!   $ gfortran -g -O0 -Wall -Wextra -Wpedantic -fbacktrace -fcheck=all \
!              -DNONRECURSIVE bst_mod.F90 bst_test.f90
!
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
    call bst_destroy( root )
end program bst_test