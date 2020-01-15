program main
    implicit none
    integer, target :: x
    integer, pointer :: p1, p2
    p1 => x
    p2 => p1
    p2 = 1
    write(*,*) x, p1, p2
    x = 2
    write(*,*) x, p1, p2
    stop
end program main
