program main
    integer :: n = 100
    integer :: i
    real :: r1 = 1.0
    real :: r2
    real :: r3 = 0
    r2 = r1/n
    do i = 0, n
        r3 = r3 + r2
    end do
    write(*,*) "r3:",r3
    n = 1024
    r3 = 0
    r2 = r1/n
    do i = 0, n
        r3 = r3 + r2
    end do
    write(*,*) "r3:",r3
    stop
end program main
