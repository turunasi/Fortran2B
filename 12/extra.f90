program main
    use autodiff
    implicit none
    integer :: t
    real(8) :: a, b, c, d, x
    call tic(t)
    c = trapezoid()
    call toc(t)
    write(*,*) 'Trapezoid:',c
    call tic(t)
    c = trapezoidwithOpenMP()
    call toc(t)
    write(*,*) 'trapezoidwithOpenMP:',c
    stop
    contains
    subroutine tic(t1)
        integer, intent(inout) :: t1
        call system_clock(t1)
    end subroutine tic
    subroutine toc(t1)
        integer, intent(in) :: t1
        integer :: t2, t_rate, t_max, diff
        call system_clock(t2,t_rate,t_max)
        if (t2 < t1) then
            diff = (t_max - t1) + t2 + 1
        else
            diff = t2 - t1
        end if
        write(*,*) "Time = ", diff/dble(t_rate)
    end subroutine toc
    function trapezoidwithOpenMP() result(S)
        integer :: n,i
        real(8) :: fx, a, b
        real(8) :: h, xi1, xi2, S
        a = 0d0
        b = 1d0
        n = 100000
        h = (b-a)/n
        S = 0d0
        xi1 = a
        xi2 = a+h
        !$omp parallel private(xi1,xi2,fx1,fx2)
        !$omp do
        do i= 1, n
            fx = (func2(xi1)+func2(xi2))/2d0
            xi1 = xi2
            xi2 = xi2+h
            !$omp atomic
            S = fx*h+S
        end do
        !$omp end do
        !$omp end parallel
    end function trapezoidwithOpenMP
    function trapezoid() result(S)
        integer :: n,i
        real(8) :: fx, a, b
        real(8) :: h, xi1, xi2, S
        a = 0d0
        b = 1d0
        n = 100000
        h = (b-a)/n
        S = 0d0
        xi1 = a
        xi2 = a+h
        do i= 1, n
            fx = (func2(xi1)+func2(xi2))/2d0
            xi1 = xi2
            xi2 = xi2+h
            S = fx*h+S
        end do
    end function trapezoid
    function func2(x) result(fx)
        real(8), intent(in) :: x
        real(8) :: fx
        fx = x ** 2d0
    end function func2
end program main
