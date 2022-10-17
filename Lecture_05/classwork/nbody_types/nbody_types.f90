program nbody_types
    implicit none
    real :: step    ! integration step
    real :: lap     ! time cycle to output coordinates
    real :: total   ! total integration time
    integer :: n    ! No of bodies
    real, allocatable :: m(:), r(:, :), v(:, :), a(:, :)
    integer :: i, j
    real :: r_ij(2), a_ij(2), a_i(2)
    real :: current, timer

    read *, step
    read *, lap
    read *, total
    read *, n
    allocate(m(n), r(2, n), v(2, n), a(2, n))
    do i = 1, n
        read *, m(i), r(:, i), v(:, i)
    end do

    do i = 1, n             ! get initial accelerations
        a_i(:) = 0.0
        do j = 1, n
            if (j == i) cycle
            r_ij(:) = r(:, j) - r(:, i)
            a_ij(:) = ( m(j) / norm2( r_ij )**3 ) * r_ij(:)
            a_i(:) = a_i(:) + a_ij(:)
        end do
        a(:, i) = a_i(:)
    end do

    print *, n              ! print the number of bodies

    current = 0.0
    timer = 0.0
    do while (current <= total)
        if (timer >= lap) then ! timer has elapsed, set it again and print coordinates
            timer = timer - lap
            do i = 1, n
                print *, r(:, i)
            end do
        end if
        v(:, :) = v(:, :) + (0.5 * step) * a(:, :)  ! half-kick velocities
        r(:, :) = r(:, :) + step * v(:, :)          ! drift coordinates
        do i = 1, n                                 ! update accelerations
            a_i(:) = 0.0
            do j = 1, n
                if (j == i) cycle
                r_ij(:) = r(:, j) - r(:, i)
                a_ij(:) = ( m(j) / norm2( r_ij )**3 ) * r_ij(:)
                a_i(:) = a_i(:) + a_ij(:)
            end do
            a(:, i) = a_i(:)
        end do
        v(:, :) = v(:, :) + (0.5 * step) * a(:, :)  ! half-kick velocities
        current = current + step                    ! next time step
        timer   = timer   + step
    end do

    deallocate(m, r, v, a)
end program nbody_types