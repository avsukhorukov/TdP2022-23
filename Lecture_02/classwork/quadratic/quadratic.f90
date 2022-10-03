program quadratic
    implicit none
    real :: a, b, c, x1, x2, d, zero

    print *, "For a quadratic equation ax^2 + bx + c = 0,"
    print *, "the coefficients a, b, and c are:"
    read *, a, b, c

    d = b**2 - 4.0 * a * c
    if (d > 0.0) then ! two distinct roots
        x1 = (-b + sqrt( d )) / (2.0 * a)
        x2 = (-b - sqrt( d )) / (2.0 * a)
        print *, "1st root is ", x1
        print *, "2nd root is ", x2

        zero = a * x1**2 + b * x1 + c
        print *, "Test 1: a x1^2 + b x1 + c = ", zero
        zero = a * x2**2 + b * x2 + c
        print *, "Test 1: a x2^2 + b x2 + c = ", zero
    else if (d == 0.0) then ! one double root
        x1 = -b / (2.0 * a)
        print *, "One double root is ", x1

        zero = a * x1**2 + b * x1 + c
        print *, "Test 1: a x1^2 + b x1 + c = ", zero
    else ! d < 0.0, no real roots
        print *, "No real roots, d = ", d
    end if
end program quadratic