program main
    use fraction
    implicit none
    integer :: n = 100
    integer :: i
    type(t_fraction) :: r1,r2,r3
    r1 = fracset(1,1)
    r2 = r1/n
    r3 = fracset(0,1)
    do i = 0, n
        r3 = r3 + r2
    end do
    write(*,*) "r3:"
    call disp(r3)
    n = 1024
    r2 = r1/n
    r3 = fracset(0,1)
    do i = 0, n
        r3 = r3 + r2
    end do
    write(*,*) "r3:"
    call disp(r3)
    stop
end program main
