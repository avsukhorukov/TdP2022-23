! This module implements basic functionality for a singly linked list that
! stores per item one integer and one pointer to the next node.  The head
! pointer is used to emulate a LIFO stack.
!
! The interface methods are coded as module procedures:
! - stack_init (nullify both pointers);
!! - stack_is_empty (test for null head);
! - stack_push (append to the head;
! - stack_pop (remove from the head);
! - stack_display (walk and print values).
! - stack_destroy
module stack_mod
    implicit none

    type :: a_sll_item
        integer :: val
        type(a_sll_item), pointer :: next
    end type a_sll_item

    interface
        subroutine callback_sub( n, data )
            integer, intent(in)     :: n
            integer, intent(in out) :: data
        end subroutine callback_sub
    end interface

    interface
        integer function callback_func( l, r )
            integer, intent(in) :: l, r
        end function callback_func
    end interface

contains

    subroutine stack_init( head )
        type(a_sll_item), pointer, intent(in out) :: head

        head => null()
    end subroutine stack_init

    ! logical function stack_is_empty( head )
    !     type(a_sll_item), pointer, intent(in) :: head
    !
    !     stack_is_empty = .not.associated( head )
    ! end function stack_is_empty

    ! Push (prepend) a new item into the list's head in the LIFO stack model.
    ! Pushing to an empty or non-empty list is the same.  There are three steps:
    !  1) create a new item taget in the heap using an extra pointer (temp);
    !  2) initialize the new item: add the value and set the next pointer to the
    !     list's head;
    !  3) point the list's head to this item to make it first in the list.
    subroutine stack_push( head, val )
        type(a_sll_item), pointer, intent(in out) :: head
        integer,                   intent(in)     :: val
        type(a_sll_item), pointer :: temp

        allocate(temp)
        temp = a_sll_item(val=val, next=head)
        head => temp
    end subroutine stack_push

    ! Pop the first value from the list's head and remove this item.  There are
    ! three steps (deletion method 2 according to A. Stolyarov):
    !  1) the temporary pointer remembers the head;
    !  2) set the head to the next item;
    !  3) destroy the temporary pointer (the 1st item).
    integer function stack_pop( head ) result(val)
        type(a_sll_item), pointer, intent(in out) :: head
        type(a_sll_item), pointer :: temp

        temp => head
        val = temp%val ! return value
        head => head%next
        deallocate(temp)
    end function stack_pop

    ! Walk through the list from head to tail and print item values.
    ! 1) point the temp pointer to the head;
    ! 2) advance the temp pointer to temp%next until temp is null();
    ! 3) while moving, print the value of the current item.
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

    ! Ugly solution using a subroutine with a callback operator and one integer
    ! to store the result.  This verion uses recursion.
    recursive subroutine stack_traverse( item, callback, data )
        type(a_sll_item), pointer, intent(in) :: item
        procedure(callback_sub) :: callback
        integer, intent(in out) :: data

        if (associated( item )) then
            call callback( item%val, data )
            call stack_traverse( item%next, callback, data )
        end if
    end subroutine stack_traverse

    recursive integer function stack_l_reduce( item, operation, seed ) result(answer)
        type(a_sll_item), pointer, intent(in) :: item
        procedure(callback_func) :: operation
        integer, intent(in) :: seed
        !integer :: new_seed

        if (associated( item )) then
            !new_seed = operation( seed, item%val )
            !answer = stack_l_reduce( item%next, operation, new_seed )
            answer = stack_l_reduce( item%next, operation, operation( seed, item%val ) )
        else
            answer = seed
        end if
    end function stack_l_reduce

    ! Destroy the stack by deleting all items.  Three methods are possible.
    ! 1) Pop all item values until the head is null;
    ! 2) Walk from head to tail by remembering the current item in the temporary
    !    pointer, setting the head to the next item, and deallocating the
    !    temporary pointer.
    ! 3) Recursively destroy the remaining part of list in the next pointer,
    !    then destroy the first item.
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
#elif defined (POP)
    subroutine stack_destroy( head )
        type(a_sll_item), pointer, intent(in out) :: head
        integer :: val

        do while (associated( head ))
            val = stack_pop( head )
        end do
    end subroutine stack_destroy
#endif

    ! Sorted insertion of new items.
#if defined (NONRECURSIVE)
    ! Insert a new item in a sorted order.  There are three cases:
    !  1) empty list (head is null());
    !  2) non-empty list, the new item is inserted before the 1st one (you must
    !     re-point the head);
    !  3) non-empty list, the new item is inserted after the 1st one.
    ! You need three extra pointers: `temp` for the target, which is inserted
    ! before item `current` and after item `previous`.
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
    ! Recursive insertion.
    ! 1) The base of recursion is the empty head.
    ! 2) For a non-empty list you either insert before the 1st element (no
    !    recursion) or after it (with recursion).
    recursive subroutine stack_insert( head, val )
        type(a_sll_item), pointer, intent(in out) :: head
        integer,                   intent(in)     :: val
        type(a_sll_item), pointer :: temp

        if (.not.associated( head )) then ! insert into the head
            allocate(head)
            head = a_sll_item(val=val, next=null())
        else if (val <= head%val) then ! insert before the 1st item
            allocate(temp)
            temp = a_sll_item(val=val, next=head)
            head => temp
        else ! insert after the 1st item recursively
            call stack_insert( head%next, val )
        end if
    end subroutine stack_insert
#endif

    ! Remove an item with the given value from a sorted stack.  Make sure that
    ! the stack is not empty.
#if defined (NONRECURSIVE)
    ! Non-recursive removal:
    ! 1) Is the value before the 1st item?
    ! 2) Walk through the list so that the searched item appears between the
    !    `previous` and `next` items (the last is included in the interval).
    !    If `next` is null(), then the value must be after the last item,
    !    that is, outside the list.  Finally, if the next value is the
    !    searched value, then remove `next` considering two cases: `next`
    !    is the 1st item (so `previous` is null()), or `next` is not the 1st
    !    item.
    subroutine stack_remove( head, val )
        type(a_sll_item), pointer, intent(in out) :: head
        integer,                   intent(in)     :: val
        type(a_sll_item), pointer :: current, temp

        if (.not.associated( head )) then   ! empty stack
            return
        else if (val < head%val) then       ! before the 1st item
            return
        else if (val == head%val) then      ! is the 1st item
            temp => head
            head => head%next
            deallocate(temp)
        else ! (val > head%val) ! Is in the %next part of the stack.
            ! Search for current%val < val <= current%next%val.  The item to remove is current%next.
            current => head
            do while (associated( current%next ))
                if (val <= current%next%val) exit   ! interval found
                current => current%next             ! otherwise advance by one item
            end do
            if (.not.associated( current%next )) then   ! after the last item
                return
            else if (current%next%val == val) then      ! value found
                temp => current%next                    ! remember the next item
                current%next => temp%next
                deallocate(temp)
            end if
        end if
    end subroutine stack_remove
#elif defined (RECURSIVE)
    ! Recursive remove:
    ! 1) The base of recursion is the empty head.  You must explicitly check
    !    this even if you test your stack before calling this subroutine as this
    !    case also happens at the end of a non-empty stack.
    ! 2) If the stack is not empty, then you either remove the 1st item (no
    !    recursion) for val == item%val, or you remove it from the remaining
    !    part of the stack (with recursion).
    recursive subroutine stack_remove( item, val )
        type(a_sll_item), pointer, intent(in out) :: item
        integer,                   intent(in)     :: val
        type(a_sll_item), pointer :: temp

        if (.not.associated( item )) then ! nothing to remove
            return
        else if (val < item%val) then ! the value is before the 1st item
            return
        else if (val == item%val) then ! the value is in the 1st item = head
            temp => item
            item => item%next
            deallocate(temp)
        else ! (val > item%val), it is in the next part of the stack
            call stack_remove( item%next, val )
        end if
    end subroutine stack_remove
#endif

end module stack_mod
