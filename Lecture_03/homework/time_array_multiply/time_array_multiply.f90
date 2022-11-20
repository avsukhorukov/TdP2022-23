program time_array_multiply
    use :: timer_class
    implicit none
    integer :: seed_size
    integer, allocatable :: seed(:)
    integer, parameter :: n = 500
    integer, parameter :: n_reps = 10
    real, dimension(n, n) :: a, b, c
    integer :: r, i, j, alpha
    real :: tmp
    type(a_timer) :: timer

    call random_seed(size=seed_size)
    allocate(seed(seed_size), source=0)
    call random_seed(put=seed)
    deallocate(seed)
    call random_number(a)
    call random_number(b)

    call timer%start()
    iaja: do r = 1, n_reps
        do i = 1, n
            do j = 1, n
                tmp = 0.0
                do alpha = 1, n
                    tmp = tmp + a(i, alpha) * b(j, alpha)
                end do ! alpha
                c(i, j) = tmp
            end do ! j
        end do ! i
    end do iaja
    call timer%stop()
    print '(a, f6.3, a)', "Contraction (i, *)(j, *) took ", timer%show() / n_reps, " s"

    call timer%start()
    iaaj: do r = 1, n_reps
        do i = 1, n
            do j = 1, n
                tmp = 0.0
                do alpha = 1, n
                    tmp = tmp + a(i, alpha) * b(alpha, j)
                end do ! alpha
                c(i, j) = tmp
            end do ! j
        end do ! i
    end do iaaj
    call timer%stop()
    print '(a, f6.3, a)', "Contraction (i, *)(*, j) took ", timer%show() / n_reps, " s"

    call timer%start()
    aija: do r = 1, n_reps
        do i = 1, n
            do j = 1, n
                tmp = 0.0
                do alpha = 1, n
                    tmp = tmp + a(alpha, i) * b(j, alpha)
                end do ! alpha
                c(i, j) = tmp
            end do ! j
        end do ! i
    end do aija
    call timer%stop()
    print '(a, f6.3, a)', "Contraction (*, i)(j, *) took ", timer%show() / n_reps, " s"

    call timer%start()
    aiaj: do r = 1, n_reps
        do i = 1, n
            do j = 1, n
                tmp = 0.0
                do alpha = 1, n
                    tmp = tmp + a(alpha, i) * b(alpha, j)
                end do ! alpha
                c(i, j) = tmp
            end do ! j
        end do ! i
    end do aiaj
    call timer%stop()
    print '(a, f6.3, a)', "Contraction (*, i)(*, j) took ", timer%show() / n_reps, " s"

    call timer%start()
    icrj: do r = 1, n_reps
        do i = 1, n
            do j = 1, n
                c(i, j) = sum( a(i, :) * b(:, j) )
            end do ! j
        end do ! i
    end do icrj
    call timer%stop()
    print '(a, f6.3, a)', "Contraction (i, :)(:, j) took ", timer%show() / n_reps, " s"

    call timer%start()
    mat_mul: do r = 1, n_reps
        c(:, :) = matmul( a(:, :), b(:, :) )
    end do mat_mul
    call timer%stop()
    print '(a, f6.3, a)', "With matmul (i, :)(:, j) took ", timer%show() / n_reps, " s"

end program time_array_multiply
