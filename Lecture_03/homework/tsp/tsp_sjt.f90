! This solution uses a non-recursive Steinhaus---Johnson---Trotter algorithm to
! generate all unique permutations of `n` towns.  I found the clearest
! description of this method in some private blog on googlesite.
!  1) Each number in the sequence has a `handedness' that shows where the
!     neighbor is.  It is either `left' or `right'.  It can be represented
!     either with a -1/+1 number (what makes the algorithm shorter but needs
!     more memory), or with a boolean flag, e.g. is_left = .true./.false., or
!     using two symbols `<' and `>'.  In textbooks this algorithm is often
!     explained using the last notation.
!  2) Definition: mobile integer is a number in the sequence, which neghbor is
!     smaller.  Utmost numbers at the ends of the sequence that have no
!     corresponding neighbors cannot be mobile integers.  For example, in a
!     sequence [<2 3> <4], only the last number is mobile.
!  3) Start from an ordered ascending sequence, [<1, <2, ..., <n].  Do algorithmic
!     checks at the beginning of an infinite loop.
!  4) Find the largest mobile integer in the sequence.  Algorithm stops if
!     none is found.
!  5) Swap this number with its neighbor and swap their handednesses as well.
!  6) Scan the sequence for numbers bigger than the largest mobile integer and
!     invert their handednesses.
!  7) Repeat from 4) again.
program travelling_salesman
    implicit none
    integer :: n, i, mobile_i, mobile_v, mi, ml, neighbor_i, distance, min_distance
    integer, allocatable :: d(:, :), route(:), min_route(:), side(:)

    print *, "The number of towns is"
    read *, n

    allocate(d(n, n))
    print *, "Distances between towns are"
    do i = 1, n
        read *, d(i, :)
    end do

    allocate(route(n))
    route(:) = [ (i, i = 1, n) ]
    allocate(min_route(n))

    allocate(side(n))
    side(:) = -1

    min_distance = huge(0)

    permutation: do
        ! For a current permutation compute the route distance.  If it is
        ! smaller than the current minimal one, remember it and remember the
        ! corresponding route.
        distance = d(route(n), route(1))
        do i = 2, n
            distance = distance + d(route(i - 1), route(i))
        end do
        if (distance < min_distance) then
            min_distance = distance
            min_route(:) = route(:)
        end if
        ! Generate the next permutation.  Find the largest mobile integer.
        mobile_i = 0 ! index
        mobile_v = 0 ! value
        largest_mobile: do i = 1, n
            neighbor_i = i + side(i) ! +-1
            if (1 <= neighbor_i .and. neighbor_i <= n) then ! inside interval [1:n]
                if (route(neighbor_i) < route(i)) then ! mobile integer by definition
                    if (mobile_v < route(i)) then
                        mobile_v = route(i)
                        mobile_i = i
                    end if
                end if
            end if
        end do largest_mobile
        if (mobile_i == 0) exit permutation ! No mobile integer found, stop permutting.
        ! Swap neighbor_i with mobile_i and their handednesses too.
        neighbor_i = mobile_i + side(mobile_i)
        mi = route(mobile_i); route(mobile_i) = route(neighbor_i); route(neighbor_i) = mi
        ml = side(mobile_i);  side(mobile_i)  = side(neighbor_i);  side(neighbor_i)  = ml
        ! Check for numbers larger than the current mobile integer and for them reverse side().
        do i = 1, n
            if (mi < route(i)) then
                side(i) = -side(i)
            end if
        end do
    end do permutation

    print "(a, i0, a)", "The shortest distance is ", min_distance, " for the following route:"
    print "(*(i3))", min_route

    deallocate(d)
    deallocate(route)
    deallocate(min_route)
    deallocate(side)
end program travelling_salesman