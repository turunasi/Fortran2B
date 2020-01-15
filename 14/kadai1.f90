program main
    implicit none
    integer, target :: i
    integer, pointer :: p
    i = 10
    p => i
    write(*,*) i, p
    p = 20
    write(*,*) i, p
    i = 30
    write(*,*) i, p
    stop
end program main
