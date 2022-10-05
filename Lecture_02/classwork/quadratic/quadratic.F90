program quadratic
    implicit none
    integer, parameter :: sp = kind( 0.e0 )
    integer, parameter :: dp = kind( 0.d0 )
    integer, parameter :: qp = kind( 0.q0 )
    integer, parameter :: wp = sp
    real(kind=wp) :: a, b, c, x1, x2, d, zero, tol
    character(len=16) :: mode
    complex(kind=wp) :: zd, z1, z2, z_zero

    print *, "For a quadratic equation ax^2 + bx + c = 0,"
    print *, "the coefficients a, b, and c are:"
    read *, a, b, c
    print *, "Which mode, 'real' or 'complex'?"
    read *, mode

    select case (mode)
    case ('real')
        d = b**2 - 4.0_wp * a * c
        tol = 10.0_wp * epsilon( d )
        if (d > tol) then ! two distinct roots
            x1 = (-b + sqrt( d )) / (2.0_wp * a)
            x2 = (-b - sqrt( d )) / (2.0_wp * a)
            print *, "1st root is ", x1
            print *, "2nd root is ", x2
#ifdef DEBUG
            zero = a * x1**2 + b * x1 + c
            print *, "Test 1: a x1^2 + b x1 + c = ", zero
            zero = a * x2**2 + b * x2 + c
            print *, "Test 1: a x2^2 + b x2 + c = ", zero
#endif
        else if (abs( d - 0.0_wp ) <= tol) then ! one double root
            x1 = -b / (2.0_wp * a)
            print *, "One double root is ", x1
#ifdef DEBUG
            zero = a * x1**2 + b * x1 + c
            print *, "Test 1: a x1^2 + b x1 + c = ", zero
#endif
        else ! d < -tol, no real roots
            print *, "No real roots, d = ", d
        end if
    case ('complex')
        zd = b**2 - 4.0_wp * a * c
        z1 = (-b + sqrt( zd )) / (2.0_wp * a)
        z2 = (-b - sqrt( zd )) / (2.0_wp * a)
        print *, "1st complex root is ", z1
        print *, "2nd complex root is ", z2
#ifdef DEBUG
        z_zero = a * z1**2 + b * z1 + c
        print *, "Test 1: a z1^2 + b z1 + c = ", z_zero
        z_zero = a * z2**2 + b * z2 + c
        print *, "Test 1: a z2^2 + b z2 + c = ", z_zero
#endif
    case default
        stop "Wrong mode '" // trim( mode ) // "'"
    end select
end program quadratic