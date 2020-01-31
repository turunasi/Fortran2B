program main
    implicit none
    integer(8) :: n,r
    write(*,*) 'Input 2integers'
    read(*,*) n,r
    write(*,*) conbi(n,r)
    stop
contains
    recursive function conbi(n,r) result(y)
        integer(8), intent(in) :: n,r
        integer(8) :: y
        if (r==1) then
            y = n
        else if (r>1) then
            y = conbi(n-1,r) + conbi(n-1,r-1)
        end if
    end function conbi
end program main
