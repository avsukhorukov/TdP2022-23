module body_mod
    use :: parallel_mod
    implicit none

    type :: a_body
        real :: m
        real :: r(2)
        real :: v(2)
        real :: a(2)
    end type a_body

contains

    subroutine body_create_datatypes()
        integer(kind=MPI_ADDRESS_KIND) :: addrs(4), lb, extent
        type(a_body) :: one_body

        call MPI_Get_address( one_body, lb )
        call MPI_Get_address( one_body%m, addrs(1) )
        call MPI_Get_address( one_body%r, addrs(2) )
        call MPI_Get_address( one_body%v, addrs(3) )
        call MPI_Get_address( one_body%a, addrs(4) )
        block
            integer :: i
            do i = 1, size( addrs )
                addrs(i) = MPI_Aint_diff( addrs(i), lb )
            end do
        end block
        ! Entire body structure.
        call MPI_Type_create_struct( 4, [1, 2, 2, 2], addrs, &
                                     [MPI_REAL, MPI_REAL, MPI_REAL, MPI_REAL], &
                                     a_body_datatype )
        ! Only the %r component.
        block
            type(MPI_Datatype) :: a_tmp
            call MPI_Type_create_struct( 1, [2], addrs([2]), [MPI_REAL], a_tmp )
            call MPI_Type_get_extent( a_body_datatype, lb, extent )
            call MPI_Type_create_resized( a_tmp, lb, extent, a_body_r_datatype )
        end block
        call MPI_Type_commit( a_body_datatype )
        call MPI_Type_commit( a_body_r_datatype )
    end subroutine body_create_datatypes

    subroutine body_free_datatypes()
        call MPI_Type_free( a_body_datatype )
        call MPI_Type_free( a_body_r_datatype )
    end subroutine body_free_datatypes

    subroutine body_init( body )
        type(a_body), intent(in out) :: body

        if (my_rank == root_rank) then
            read *, body%m, body%r(:), body%v(:)
            body%a(:) = [0.0, 0.0]
        end if
    end subroutine body_init

    subroutine body_print_cooridinates( body )
        type(a_body), intent(in) :: body

        if (my_rank == root_rank) then
            print "(2(es11.3, 1x))", body%r(:)
        end if
    end subroutine body_print_cooridinates

    subroutine body_half_kick( body, step )
        type(a_body), intent(in out) :: body
        real,         intent(in)     :: step

        body%v(:) = body%v(:) + (0.5 * step) * body%a(:)
    end subroutine body_half_kick

    subroutine body_drift( body, step )
        type(a_body), intent(in out) :: body
        real,         intent(in)     :: step

        body%r(:) = body%r(:) + step * body%v(:)
    end subroutine body_drift

    function body_acceleration_from( body, other_body ) result(a_ij)
        real, dimension(2) :: a_ij
        type(a_body), intent(in) :: body, other_body
        real, dimension(2) :: r_ij

        r_ij(:) = other_body%r(:) - body%r(:)
        a_ij(:) = ( other_body%m / norm2( r_ij(:) )**3 ) * r_ij(:)
    end function body_acceleration_from

end module body_mod
