program main
    implicit none
    type bunnsuu
        integer :: bunbo, bunsi
    end type bunnsuu
    type(bunnsuu) :: A
    A%bunsi = 8
    A%bunbo = 66
    write(*,*) A%bunsi, "/", A%bunbo, "=", REAL(A%bunsi)/REAL(A%bunbo)
end program main
