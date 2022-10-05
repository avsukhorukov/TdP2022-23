program time_array_add
    implicit none
    integer, parameter :: n_reps = 8
    integer :: n, i, j, k, r
    real, allocatable, dimension(:, :, :) :: a, b
    real :: t1, t2

    print *, "The array size n is "
    read *, n
    allocate(a(n, n, n), b(n, n, n))

    call random_number( a )
    call random_number( b )

    call cpu_time( t1 )
    kji: do r = 1, n_reps
        do i = 1, n
            do j = 1, n
                do k = 1, n
                    a(i, j, k) = a(i, j, k) + b(i, j, k)
                end do
            end do
        end do
    end do kji
    call cpu_time( t2 )
    print *, "Order k-j-i took ", (t2 - t1) / n_reps, " s"

    call cpu_time( t1 )
    ijk: do r = 1, n_reps
        do k = 1, n
            do j = 1, n
                do i = 1, n
                    a(i, j, k) = a(i, j, k) + b(i, j, k)
                end do
            end do
        end do
    end do ijk
    call cpu_time( t2 )
    print *, "Order i-j-k took ", (t2 - t1) / n_reps, " s"

    call cpu_time( t1 )
    vector: do r = 1, n_reps
        a = a + b
    end do vector
    call cpu_time( t2 )
    print *, "Vector sum took ", (t2 - t1) / n_reps, " s"

    deallocate(a, b)
end program time_array_add