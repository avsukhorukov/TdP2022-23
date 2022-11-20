module sorted_stack_ref_mod
    implicit none

    type :: a_sll_ref
        type(a_sll_item), pointer :: ref
    end type a_sll_ref

    type :: a_sll_item
        integer :: val
        type(a_sll_ref) :: next
    end type a_sll_item

contains

    subroutine sorted_stack_init( head )
        type(a_sll_ref), intent(in out) :: head

        head%ref => null()
    end subroutine sorted_stack_init

    logical function sorted_stack_is_empty( head )
        type(a_sll_ref), intent(in) :: head

        sorted_stack_is_empty = .not.associated( head%ref )
    end function sorted_stack_is_empty

    ! Insert a new item in a sorted order.
    subroutine sorted_stack_insert( head, val )
        type(a_sll_ref), target, intent(in out) :: head
        integer,                 intent(in)     :: val
        !
        type(a_sll_ref),  pointer :: current
        type(a_sll_item), pointer :: tail

        ! Search for the current item to insert into it.
        current => head
        next_item: do while (associated( current%ref ))
            if (current%ref%val < val) then ! advance by one item
                current => current%ref%next
            else
                exit next_item
            end if
        end do next_item
        tail => current%ref ! remember the tail
        allocate(current%ref)
        current%ref = a_sll_item(val=val, next=a_sll_ref(ref=tail))
    end subroutine sorted_stack_insert

    ! Removing a item from a sorted stack for the given value `val` assuming
    ! that the stack is not empty (this is checked before calling this
    ! subroutine.
    subroutine sorted_stack_remove( head, val )
        type(a_sll_ref), target, intent(in out) :: head
        integer,                 intent(in)     :: val
        !
        type(a_sll_ref),  pointer :: current
        type(a_sll_item), pointer :: tail

        current => head
        next_item: do while (associated(current%ref)) ! search for current%val >= val
            if (current%ref%val < val) then
                current => current%ref%next
            else
                exit next_item
            end if
        end do next_item
        if (.not.associated(current%ref)) return ! empty or after the last item
        if (current%ref%val == val) then ! found
            tail => current%ref%next%ref ! remember the tail
            deallocate(current%ref)
            current%ref => tail
        end if
        return
    end subroutine sorted_stack_remove

    subroutine sorted_stack_display(head)
        type(a_sll_ref), target, intent(in) :: head
        !
        type(a_sll_ref), pointer :: temp

        temp => head
        do while (associated(temp%ref))
            write (*, "(a, i0, a)", advance="no") "(", temp%ref%val, ")->"
            temp => temp%ref%next
        end do
        print "(a)", "null()"
    end subroutine sorted_stack_display

    subroutine sorted_stack_destroy(head)
        type(a_sll_ref), intent(in out) :: head
        !
        type(a_sll_item), pointer :: temp

        do
            if (sorted_stack_is_empty(head)) exit
            temp => head%ref%next%ref
            deallocate(head%ref)
            head%ref => temp
        end do
    end subroutine sorted_stack_destroy

end module sorted_stack_ref_mod