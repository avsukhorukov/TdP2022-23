module stack_mod
    implicit none

    type :: a_sll_item
        integer :: val
        type(a_sll_item), pointer :: next
    end type a_sll_item

contains

    subroutine stack_push( head, val )
        type(a_sll_item), pointer, intent(in out) :: head
        integer,                   intent(in)     :: val
        type(a_sll_item), pointer :: temp

        allocate(temp)
        temp%val = val
        temp%next => head
        !temp = a_sll_item(val=val, next=head)
        head => temp
    end subroutine stack_push

    subroutine stack_display( head )
        type(a_sll_item), pointer, intent(in) :: head
        type(a_sll_item), pointer :: temp

        temp => head
        do while (associated( temp ))
            write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
            temp => temp%next
        end do
        print "(a)", "null()"
    end subroutine stack_display

    integer function stack_pop( head ) result(val)
        type(a_sll_item), pointer, intent(in out) :: head
        type(a_sll_item), pointer :: temp

        temp => head
        val = temp%val ! return value
        head => head%next
        deallocate(temp)
    end function stack_pop

#if defined (NONRECURSIVE)
    subroutine stack_destroy( head )
        type(a_sll_item), pointer, intent(in out) :: head
        type(a_sll_item), pointer :: temp

        do while (associated( head ))
            temp => head                ! remember the first item
            head => head%next           ! advance the head one item forward
            deallocate(temp)            ! destroy the first item
        end do
    end subroutine stack_destroy
#elif defined (RECURSIVE)
    recursive subroutine stack_destroy( item )
        type(a_sll_item), pointer, intent(in out) :: item

        if (associated( item )) then
            call stack_destroy( item%next )
            deallocate(item)
        end if
    end subroutine stack_destroy
#endif

#if defined (NONRECURSIVE)
    subroutine stack_insert( head, val )
        type(a_sll_item), pointer, intent(in out) :: head
        integer,                   intent(in)     :: val
        type(a_sll_item), pointer :: temp, current

        allocate(temp)
        temp = a_sll_item(val=val, next=null())

        if (.not.associated( head )) then ! stack is empty
            head => temp
        else if (val < head%val) then ! stack is not empty: insert before the 1st item
            temp%next => head
            head => temp
        else ! (val >= head%val) ! stack is not empty: insert somewhere after the 1st item
            current => head
            do while (associated( current%next ))
                if (val < current%next%val) exit    ! found
                current => current%next             ! otherwise advance by one item
            end do
            temp%next => current%next
            current%next => temp
        end if
    end subroutine stack_insert
#elif defined (RECURSIVE)
    recursive subroutine stack_insert( head, val )
        type(a_sll_item), pointer, intent(in out) :: head
        integer,                   intent(in)     :: val
        type(a_sll_item), pointer :: temp

        ! Homework
    end subroutine stack_insert
#endif

#if defined (NONRECURSIVE)
    subroutine stack_remove( head, val )
        type(a_sll_item), pointer, intent(in out) :: head
        integer,                   intent(in)     :: val
        type(a_sll_item), pointer :: previous, current

        if (.not.associated( head )) return         ! empty stack
        if (val < head%val) return                  ! before the 1st item
        previous => null()
        current  => head
        next_item: do while (associated( current )) ! search for previous%val < val <= current%val
            if (current%val < val) then
                previous => current
                current  => current%next
            else
                exit next_item
            end if
        end do next_item
        if (.not.associated( current )) return      ! after the last item
        if (current%val == val) then                ! found
            if (associated( previous )) then        ! current is not the 1st item
                previous%next => current%next
            else                                    ! current is the 1st item
                head => current%next
            end if
            deallocate(current)
        end if
    end subroutine stack_remove
#elif defined (RECURSIVE)
    recursive subroutine stack_remove( item, val )
        type(a_sll_item), pointer, intent(in out) :: item
        integer,                   intent(in)     :: val
        type(a_sll_item), pointer :: temp

        ! Homework
    end subroutine stack_remove
#endif

#if defined (NONRECURSIVE)
    function stack_search( head, val ) result(found)
        type(a_sll_item), pointer :: found
        type(a_sll_item), pointer, intent(in) :: head
        integer,                   intent(in) :: val

        found => head
        do while (associated( found ))
            if (val == found%val) exit
            found => found%next
        end do
    end function stack_search
#elif defined (RECURSIVE)
    recursive function stack_search( item, val ) result(found)
        type(a_sll_item), pointer :: found
        type(a_sll_item), pointer, intent(in) :: item
        integer,                   intent(in) :: val

        found => item
        if (associated( found )) then
            if (val == found%val) return
            found => stack_search( item%next, val )
        end if
    end function stack_search
#endif


end module stack_mod