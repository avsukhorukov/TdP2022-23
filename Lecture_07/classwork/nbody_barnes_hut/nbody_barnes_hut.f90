program nbody_barnes_hut
    use :: clock_class
    use :: bodies_mod, only: bodies_init, bodies_get_size, &
        bodies_print_cooridinates, bodies_half_kick, bodies_drift, &
        bodies_destroy
    use :: tree_mod, only: tree_create, tree_accelerate, tree_destroy
    implicit none
    type(a_clock) :: clock
    real :: step

    call clock%init()
    call bodies_init()
    call tree_create()
    call tree_accelerate()

    print *, bodies_get_size()
    step = clock%get_step()

    do while (clock%time_is_not_over())
        if (clock%timer_has_elapsed()) then
            call bodies_print_cooridinates()
        end if
        call bodies_half_kick( step )
        call bodies_drift( step )
        call tree_destroy()
        call tree_create()
        call tree_accelerate()
        call bodies_half_kick( step )
        call clock%next_step()
    end do

    call tree_destroy()
    call bodies_destroy()
end program nbody_barnes_hut