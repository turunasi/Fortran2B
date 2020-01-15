program main
    implicit none
    integer, pointer :: p, q(:)
    allocate(p, q(3))
    p = 3
    q(:) = 2
    write(*,*) p
    write(*,*) q
    stop
end program main
