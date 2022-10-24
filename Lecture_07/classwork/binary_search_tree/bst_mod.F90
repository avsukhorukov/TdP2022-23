module bst_mod
    implicit none

    type :: a_bst_node
        integer :: val
        type(a_bst_node), pointer :: left
        type(a_bst_node), pointer :: right
    end type a_bst_node

contains

#if defined (RECURSIVE)
    recursive subroutine bst_insert( node, val )
        type(a_bst_node), pointer, intent(in out) :: node
        integer,                   intent(in)     :: val

        if (.not.associated( node )) then
            allocate(node)
            node = a_bst_node(val=val, left=null(), right=null())
        else if (val < node%val) then
            call bst_insert( node%left, val )
        else if (val > node%val) then
            call bst_insert( node%right, val )
        else ! val == node%val, node already exists, skip it.
            continue
        end if
    end subroutine bst_insert
#elif defined (NONRECURSIVE)
    ! Homework: non-recursive insertion.
    subroutine bst_insert( root, val )
        type(a_bst_node), pointer, intent(in out) :: root
        integer,                   intent(in)     :: val
        type(a_bst_node), pointer :: current, previous, temp

        previous => null()
        current  => root
        ! ...
        allocate(temp)
        temp = a_bst_node(val=val, left=null(), right=null())
        ! ...
    end subroutine bst_insert
#elif defined (SEARCHED)
    ! This is not working as expected because a pointer to 0x0 is always
    ! returned and when a new target is allocated in `temp`, it is not saved in
    ! the tree.
    subroutine bst_insert( root, val )
        type(a_bst_node), pointer, intent(in out) :: root
        integer,                   intent(in)     :: val
        type(a_bst_node), pointer :: temp

        temp => bst_search( root, val )
        if (.not.associated( temp )) then
            ! This is not working as allocating 0x0 produces a target,
            ! disconnected from the tree.
            allocate(temp)
            temp = a_bst_node(val=val, left=null(), right=null())
        else
            continue ! do nothing if the node already exists
        end if
    end subroutine bst_insert
#endif

    ! This is in-order tree walk.
    recursive subroutine bst_display( node )
        type(a_bst_node), pointer, intent(in) :: node

        if (associated( node )) then
            call bst_display( node%left )
            print "(i0)", node%val
            call bst_display( node%right )
        end if
    end subroutine bst_display

    recursive subroutine bst_destroy( node )
        type(a_bst_node), pointer, intent(in out) :: node

        if (associated( node )) then
            call bst_destroy( node%left )
            call bst_destroy( node%right )
            deallocate(node)
        end if
    end subroutine bst_destroy

#if defined (RECURSIVE)
    recursive logical function bst_has( node, val ) result(answer)
        type(a_bst_node), pointer, intent(in) :: node
        integer,                   intent(in) :: val

        if (.not.associated( node )) then
            answer = .false.
        else if (val < node%val) then
            answer = bst_has( node%left, val )
        else if (val > node%val) then
            answer = bst_has( node%right, val )
        else ! (val == node%val) ! node exists
            answer = .true.
        end if
    end function bst_has
#elif defined (NONRECURSIVE)
    recursive logical function bst_has( root, val ) result(answer)
        type(a_bst_node), pointer, intent(in) :: root
        integer,                   intent(in) :: val

        answer = associated( bst_search( root, val ) )
    end function bst_has
#endif

#if defined (RECURSIVE)
    ! Search the tree starting from the `node` for the given `val` and return
    ! a pointer to the node with this value, if it exists, or null(), if it
    ! doesn't.
    recursive function bst_search( node, val ) result(found)
        type(a_bst_node), pointer :: found
        type(a_bst_node), pointer, intent(in) :: node
        integer,                   intent(in) :: val

        if (.not.associated( node )) then
            found => node ! Warning: this pointer is 0x0, so null will be returned
        else if (val < node%val) then
            found => bst_search( node%left, val )
        else if (val > node%val) then
            found => bst_search( node%right, val )
        else ! (val == node%val)
            found => node
        end if
    end function bst_search
#elif defined (NONRECURSIVE)
    ! Homework: non-recursive version of `bst_search()`.
    function bst_search( root, val ) result(current)
        type(a_bst_node), pointer :: current
        type(a_bst_node), pointer, intent(in) :: root
        integer,                   intent(in) :: val

        current => root
        do
            ! ...
        end do
    end function bst_search
#endif

    recursive integer function bst_sum( node ) result(answer)
        type(a_bst_node), pointer, intent(in) :: node

        if (associated( node )) then
            answer = node%val + bst_sum( node%left ) + bst_sum( node%right )
        else
            answer = 0
        end if
    end function bst_sum

    ! Homework: the size of a tree is the total number of nodes in it.
    recursive integer function bst_size( node ) result(n_nodes)
        type(a_bst_node), pointer, intent(in) :: node

        ! ...
    end function bst_size

    ! Homework: the height of the node is the largest number of edges from this
    ! node down to a leaf (a node with no children).
    recursive integer function bst_height( node ) result(height)
        type(a_bst_node), pointer, intent(in) :: node

        ! ...
    end function bst_height

    ! Homework: for a tree given by the `root` pointer follow all left/right
    ! children until there are no more left and return a pointer to this node
    ! holding the minimum/maximum value.
    !
    ! This is a generic version that works both with min and max.
    function bst_find_minmax( root, which ) result(extremum)
        type(a_bst_node), pointer :: extremum
        type(a_bst_node), pointer, intent(in) :: root
        character(len=*),          intent(in) :: which ! `min` or `max`

        ! ...
    end function bst_find_minmax

end module bst_mod