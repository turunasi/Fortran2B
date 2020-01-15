program main
    implicit none
    integer, target :: x(6)
    integer, pointer :: p(:)
    x(:) = 0
    p => x(3:5)
    p(1) = 1
    write(*,*) x(1:3)
    p => x
    p(1) = 2
    write(*,*) x(1:3)
    stop
end program main
