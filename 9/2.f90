program main
    use mymod
    implicit none
    type(bunnsuu) :: A
    A%bunsi = 8
    A%bunbo = 66
    write(*,*) A%bunsi, "/", A%bunbo, "=", REAL(A%bunsi)/REAL(A%bunbo)
end program main
