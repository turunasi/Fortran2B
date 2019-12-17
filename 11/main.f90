program main
    use autodiff
    implicit none
    real(8) :: a, b, c, d, x
    x = 5d0
    write(*,*) 'Input real number'
    read(*,*) d
    if (d.lt.0) then
      write (*,*) 'Error: value is minus'
      stop
    end if
    a = newton(x,d)
    b = midpoint()
    c = trapezoid()
    write(*,*) 'Newton method:',a
    write(*,*) 'Midpoint:',b
    write(*,*) 'Trapezoid:',c
    stop
    contains

    function newton(x,d) result(x0)
        real(8) :: x,d
        real(8) :: x0
        real(8) :: f, fd,eps 
        type(diff) :: fx
        eps = 10e-14
        x0 = x
        do
            fx = func1(x0) - d
            f = outx(fx)
            fd = outdx(fx)
            x0 = x0 - f/fd
            if (abs(f/fd).lt.eps) then
                exit
            end if
        end do
    end function newton

    function midpoint() result(S)
        integer :: n,i
        real(8) :: fx, a, b
        real(8) :: h, xi1, xi2, S
        a = 0d0
        b = 1d0
        n = 3000
        h = (b-a)/n
        S = 0d0
        xi1 = a
        xi2 = a+h
        do i= 1, n
            fx = func2(xi1+(xi2-xi1)/2d0)
            S = fx*h+S
            xi1 = xi2
            xi2 = xi2+h
        end do
    end function midpoint
    function trapezoid() result(S)
        integer :: n,i
        real(8) :: fx, a, b
        real(8) :: h, xi1, xi2, S
        a = 0d0
        b = 1d0
        n = 3000
        h = (b-a)/n
        S = 0d0
        xi1 = a
        xi2 = a+h
        do i= 1, n
            fx = (func2(xi1)+func2(xi2))/2d0
            S = fx*h+S
            xi1 = xi2
            xi2 = xi2+h
        end do
    end function trapezoid
    function func1(x0) result(fx0)
        real(8), intent(in) :: x0
        type(diff) :: xd, fx0
        xd = diffset(x0)
        fx0 = xd * xd
    end function func1
    function func2(x) result(fx)
        real(8), intent(in) :: x
        real(8) :: fx
        fx = x ** 2d0 
    end function func2
end program main
