program sll_3items
    implicit none

    type :: a_sll_item
        integer :: val
        type(a_sll_item), pointer :: next
    end type a_sll_item

    type(a_sll_item), pointer :: head

    head => null()
    ! nullify(head)

    allocate(head)
    head%val = 1
    head%next => null()

    allocate(head%next)
    head%next%val = 2
    head%next%next => null()

    allocate(head%next%next)
    head%next%next%val = 3
    head%next%next%next => null()

    deallocate(head%next%next)
    deallocate(head%next)
    deallocate(head)
end program sll_3items