! A basic functionality for a singly-linked list that is managed with the head
! and tail pointers to emulate a FIFO queue.  The following methods are provided
! for this class:
! - queue_init (nullify both pointers);
! - queue_is_empty (test for null head);
! - queue_put (append to the tail);
! - queue_get (remove from the head);
! - queue_display (walk and print values);
! - queue_destroy (get and free all items).
module queue_class
    implicit none
    private

    type :: a_sll_item
        integer :: val
        type(a_sll_item), pointer :: next => null()
    end type a_sll_item

    type, public :: a_queue
        type(a_sll_item), pointer :: head => null()
        type(a_sll_item), pointer :: tail => null()
    contains
        procedure, public :: init     => queue_init
        procedure, public :: is_empty => queue_is_empty
        procedure, public :: put      => queue_put
        procedure, public :: get      => queue_get
        procedure, public :: display  => queue_display
    end type a_queue

contains

    subroutine queue_init( self )
        class(a_queue), intent(in out) :: self

        self%head => null()
        self%tail => null()
        return
    end subroutine queue_init

    logical function queue_is_empty( self )
        class(a_queue), intent(in) :: self

        queue_is_empty = .not.associated( self%head )
        return
    end function queue_is_empty

    ! A new item is appended to the tail.  If the list is empty, then both the
    ! head and the tail are pointed to it.  If the list is not empty, then i
    !  becomes the next component of the tail, which is then pointed to it.
    subroutine queue_put( self, val )
        class(a_queue), intent(in out) :: self
        integer,        intent(in)     :: val
        type(a_sll_item), pointer :: temp

        ! Get a new target item.
        allocate(temp)
        temp = a_sll_item(val=val, next=null())
        if (associated( self%head )) then   ! queue is not empty
            self%tail%next => temp
        else                                ! queue is empty
            self%head => temp
        end if
        self%tail => temp
    end subroutine queue_put

    ! Get the first item from the head and return its value.
    integer function queue_get( self ) result(answer)
        class(a_queue), intent(in out) :: self
        type(a_sll_item), pointer :: temp

        answer = self%head%val ! return value
        temp      => self%head
        self%head => self%head%next
        deallocate(temp)
        ! Once the last item has been removed, the head is empty so must be the
        ! tail.
        if (.not.associated( self%head )) self%tail => null()
    end function queue_get

    subroutine queue_display( self )
        class(a_queue), intent(in) :: self
        type(a_sll_item), pointer :: temp

        temp => self%head
        do while (associated( temp ))
            write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
            temp => temp%next
        end do
        print "(a)", "null()"
    end subroutine queue_display
end module queue_class