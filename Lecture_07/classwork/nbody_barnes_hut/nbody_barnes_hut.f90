program nbody_barnes_hut
    use :: clock_class
    use :: bodies_mod
    implicit none
    type(a_clock) :: clock
    real :: step

    call clock%init()
    call bodies_init()
    call bodies_accelerate()

    print *, bodies_get_size()
    step = clock%get_step()

    do while (clock%time_is_not_over())
        if (clock%timer_has_elapsed()) then
            call bodies_print_cooridinates()
        end if
        call bodies_half_kick( step )
        call bodies_drift( step )
        call bodies_accelerate()
        call bodies_half_kick( step )
        call clock%next_step()
    end do

    call bodies_destroy()
end program nbody_barnes_hut