program main
    implicit none
    write(*,*) fact(3)
    stop
contains
    recursive function fact(n) result(y)
        integer, intent(in) :: n
        integer :: y
        if (n==1) then
            y = 1
        else if (n > 1) then
            y = n*fact(n-1)
        end if
    end function fact
end program main
