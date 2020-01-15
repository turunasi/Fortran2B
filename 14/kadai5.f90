program main
    implicit none
    integer, pointer :: p
    integer, target :: x
    p => x
    write(*,*) associated(p)
    nullify(p)
    write(*,*) associated(p)
    allocate(p)
    write(*,*) associated(p)
    deallocate(p)
    write(*,*) associated(p)
    stop
end program main
