! This implements class functionality of a LIFO stack with a singly-linked list,
! that stores one integer and has a single pointer to the next item.
!
! Note: final procedure is not working right here.  According to the standard,
! finalization is called for the corresponding target once a pointer is
! deallocated.  Pointer temp is deallocated at the end of stack_pop().  This
! single deallocation calls the final procedure and removes the entire list
! recursively.
module stack_class
    implicit none
    private

    type :: a_sll_item
        integer :: val
        type(a_sll_item), pointer :: next
    !contains
    !    final :: item_destroy
    end type a_sll_item

    type, public :: a_stack
        type(a_sll_item), pointer :: head
    contains
        procedure, public :: init     => stack_init
        procedure, public :: is_empty => stack_is_empty
        procedure, public :: push     => stack_push
        procedure, public :: pop      => stack_pop
        procedure, public :: display  => stack_display
        procedure, public :: destroy  => stack_destroy
    end type a_stack

contains

    subroutine stack_init( self )
        class(a_stack), intent(in out) :: self

        self%head => null()
    end subroutine stack_init

    logical function stack_is_empty( self )
        class(a_stack), intent(in) :: self

        stack_is_empty = .not.associated( self%head )
    end function stack_is_empty

    ! Push the given value into the new item at the head of the stack.
    subroutine stack_push( self, val )
        class(a_stack), intent(in out) :: self
        integer,        intent(in)     :: val
        !
        type(a_sll_item), pointer :: temp

        allocate(temp)
        temp = a_sll_item(val=val, next=self%head)
        self%head => temp
    end subroutine stack_push

    ! Popping from an empty stack is an error. Before popping, check for the
    ! emptiness using the stack_is_empty() method.
    integer function stack_pop( self ) result(answer)
        class(a_stack), intent(in out) :: self
        !
        type(a_sll_item), pointer :: temp

        temp      => self%head
        self%head => self%head%next
        answer = temp%val
        deallocate(temp)
    end function stack_pop

    subroutine stack_display( self )
        class(a_stack), intent(in) :: self
        !
        type(a_sll_item), pointer :: temp

        temp => self%head
        do while (associated( temp ))
            write(*, "(a, i0, a)", advance="no") "(", temp%val, ")->"
            temp => temp%next
        end do
        print "(a)", "null()"
    end subroutine stack_display

    subroutine stack_destroy( self )
        class(a_stack), intent(in out) :: self
        type(a_sll_item), pointer :: temp

        do while (associated( self%head ))
            temp => self%head               ! remember the current item
            self%head => self%head%next     ! advance the head one item forward
            deallocate(temp)                ! destroy the current item
        end do
    end subroutine stack_destroy

    ! ! Destructor for a list item.
    ! recursive subroutine item_destroy( self )
    !     type(a_sll_item), intent(in out) :: self

    !     if (associated( self%next )) then
    !         deallocate(self%next)
    !     end if
    ! end subroutine item_destroy
end module stack_class