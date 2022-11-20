! You cannot use `digits` as a variable because this is an intrinsic function.
! n_perms at the end must give the factorial of n_digs.
program quickperm
    implicit none
    integer :: n, n_digs, n_perms, d, tmp, r
    integer, allocatable :: digs(:)

    print '(a)', "The number is"
    read *, n

    ! Decompose it into digits
    n_digs = floor(log10(real(n)) + 1)
    allocate(digs(n_digs))
    tmp = n
    do d = 1, n_digs
        r = mod(tmp, 10)
        digs(n_digs + 1 - d) = r
        tmp = (tmp - r) / 10
    end do

    n_perms = 0
    call permutate(digs, size(digs))
    print '(i0, a)', n_perms, " permutations"
    print '(i0, a)', n_swaps, " swaps"
    deallocate(digs)
contains
    subroutine swap(a, b)
        integer, intent(inout) :: a, b
        integer :: tmp
        tmp = a; a = b; b = tmp
    end subroutine swap

    ! This is a recursive variant of QuickPerm algorithm.  We start from the end
    ! of the input array (m = size(a)) and perutate the subarray a(1:m - 1)
    ! recursively.  The original formulation uses base-0 indexing, here both
    ! indices and the logical condition are corrected to work with base-1
    ! indexing.  Here is the idea: first permute the subarray, then loop from
    ! the one before last element (m - 1) to the first element (1).  If m is
    ! odd, then each time swap the first with the last (m) element, otherwise if
    ! m is even, then swap the current element in the loop with the last
    ! element.  After swapping, permute again.
    recursive subroutine permutate(a, m)
        integer, intent(inout) :: a(:)
        integer, intent(in)    :: m
        integer                :: i, j
        if (m == 1) then
            print '(*(i1))', a(:)
            n_perms = n_perms + 1
        else
            call permutate(a, m - 1)
            do i = m - 1, 1, -1 ! from m - 1 through 1.
                if (mod(m, 2) == 0) then ! even m
                    j = i
                else ! odd m
                    j = 1
                end if
                call swap(a(j), a(m))
                call permutate(a, m - 1)
            end do
        end if
    end subroutine permutate
end program quickperm