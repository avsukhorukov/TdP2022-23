! Compilation order:
!
! $ mpifort -g -O0 -Wall -Wextra -Wpedantic -fbacktrace -fcheck=all \
!   parallel_mod.f90 clock_class.f90 body_mod.f90 node_mod.f90 bodies_mod.f90 \
!   tree_mod.f90 nbody_barnes_hut.f90 
!
program nbody_barnes_hut
    use :: parallel_mod, only : init_parallel, finish_parallel, my_rank, root_rank
    use :: clock_class
    use :: bodies_mod, only : bodies_init, bodies_get_size, &
           bodies_print_cooridinates, bodies_half_kick, bodies_drift, &
           bodies_destroy
    use :: tree_mod, only : tree_create, tree_accelerate, tree_destroy
    implicit none
    type(a_clock) :: clock
    real :: step

    call init_parallel()
    call clock%init()
    call bodies_init()
    call tree_create()
    call tree_accelerate()

    if (my_rank == root_rank) print *, bodies_get_size()
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
    call finish_parallel()
end program nbody_barnes_hut