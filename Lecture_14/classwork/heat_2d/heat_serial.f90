! Heat transport in 2D
!
! Parallelize the following serial code.
!
! $ gfortran -g -O0 -Wall -Wextra -Wpedantic -fcheck=all -fbacktrace
!   heat_serial.f90
!
! $ ./a.out < in10.txt
program heat_serial
    implicit none
    real, dimension(:, :), pointer :: prev_t, next_t, temp_t
    integer :: s, time
    real :: diff_t

    read *, s
    allocate(prev_t(0:s + 1, 0:s + 1))
    allocate(next_t(0:s + 1, 0:s + 1))

    call read_temperature( prev_t, s )
    call update_borders( prev_t, next_t, s )

    time = 0
    do
        call next_iteration( prev_t, next_t, s )
        diff_t = maxval( abs( next_t(1:s, 1:s) - prev_t(1:s, 1:s) ) )
        if (modulo( time, 5000 ) == 0) then
            print "(a, i0, a, es8.2)", "time = ", time, ", diff_t = ", diff_t
            call print_temperature( prev_t, s )
            ! call wait_cls( 100 )
        end if
        if (diff_t < epsilon( 0.0 )) exit
        temp_t => prev_t;  prev_t => next_t;  next_t => temp_t
        time = time + 1
    end do
    print "(a, i0)", "Final time is ", time
    deallocate(prev_t)
    deallocate(next_t)

contains

    ! Read the entire 2D array from input including all four boundaries at
    ! i = 0 and i = side + 1, as well as at j = 0 and j = side + 1.
    subroutine read_temperature( mtx, side )
        real, dimension(:, :), pointer, intent(in out) :: mtx
        integer,                        intent(in)     :: side
        integer :: i

        do i = 0, s + 1
            read *, mtx(i, 0:side + 1)
        end do
    end subroutine read_temperature

    ! Print the entire 2D array to stdout including all four boundaries.
    subroutine print_temperature( mtx, side )
        real, dimension(:, :), pointer, intent(in out) :: mtx
        integer,                        intent(in)     :: side
        integer :: i

        do i = 0, s + 1
            print "(*(f5.0))", mtx(i, 0:side + 1)
        end do
    end subroutine print_temperature

    ! Wait specified number of ms and then clear the terminal screen.
    subroutine wait_cls( ms )
       integer, intent(in) :: ms
       integer :: tick, tack
       real :: rate
       call system_clock( count=tick, count_rate=rate )
       do
           call system_clock( count=tack )
           if (real( tack - tick ) / rate >= ms * 1e-3) exit
       end do
       ! Clear the terminal screen using the console escape code ^[2J.
       print "(2a)", achar( 27 ), "[2J"
    end subroutine wait_cls

    subroutine next_iteration( old_mtx, new_mtx, side )
        real, dimension(:, :), pointer, intent(in)     :: old_mtx
        real, dimension(:, :), pointer, intent(in out) :: new_mtx
        integer,                        intent(in)     :: side
        !
        real, parameter :: alpha = 2.5e-4
        
        new_mtx(1:side, 1:side) = &
            (1.0 - 4.0 * alpha) *     old_mtx(1:side,     1:side)     &
                       + alpha  * (   old_mtx(0:side - 1, 1:side    ) &
                                    + old_mtx(2:side + 1, 1:side    ) &
                                    + old_mtx(1:side    , 0:side - 1) &
                                    + old_mtx(1:side    , 2:side + 1) )
    end subroutine next_iteration

    subroutine update_borders( old_mtx, new_mtx, side )
        real, dimension(:, :), pointer, intent(in)     :: old_mtx
        real, dimension(:, :), pointer, intent(in out) :: new_mtx
        integer,                        intent(in)     :: side
        
        ! Inner columns:
        new_mtx(1:side, 0       ) = old_mtx(1:side, 0       )
        new_mtx(1:side, side + 1) = old_mtx(1:side, side + 1)
        ! Full rows:
        new_mtx(0,        0:side + 1) = old_mtx(0,        0:side + 1)
        new_mtx(side + 1, 0:side + 1) = old_mtx(side + 1, 0:side + 1)
    end subroutine update_borders

end program heat_serial
