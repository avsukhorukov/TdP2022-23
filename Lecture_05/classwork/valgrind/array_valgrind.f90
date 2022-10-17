program array_valgrind
    integer, parameter :: n = 5
    real, allocatable :: a(:)
    integer :: i
    integer, pointer :: iptr

    allocate(a(n))
    a = [ (real(i), i = 1, n) ]
    do i = 1, n
        a(i) = a(i + 1)**2 
    end do

    allocate(iptr)
    allocate(iptr)
end program array_valgrind